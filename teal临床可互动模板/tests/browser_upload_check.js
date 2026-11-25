// Native teal/Shiny upload smoke test using a temporary Edge profile and local CDP.
// Run: node tests/browser_upload_check.js http://127.0.0.1:18765 ADSL.csv ADAE.csv
// Optional env: TEAL_TEST_MAP='{"ADSL":{"USUBJID":"SUBJECT","SITEID":"CENTER","TRT01P":"ARM"}}'
// TEAL_TEST_ADSL_N / TEAL_TEST_ADAE_N: expected row counts (otherwise inferred from CSV).
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
const assert = require('node:assert/strict');
const { spawn } = require('node:child_process');

const [url, adslArg, adaeArg] = process.argv.slice(2);
if (!url || !adslArg || !adaeArg) throw new Error('Usage: node tests/browser_upload_check.js <url> <ADSL file> <ADAE file>');
const files = { ADSL: path.resolve(adslArg), ADAE: path.resolve(adaeArg) };
for (const file of Object.values(files)) assert.ok(fs.existsSync(file), `Missing test fixture: ${file}`);
const mappings = JSON.parse(process.env.TEAL_TEST_MAP || '{}');
const edge = process.env.TEAL_TEST_EDGE || 'C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe';
const profile = fs.mkdtempSync(path.join(os.tmpdir(), 'teal-upload-'));
const browser = spawn(edge, ['--headless=new', '--disable-gpu', '--no-first-run', '--no-default-browser-check', '--no-proxy-server',
  '--remote-debugging-port=0', `--user-data-dir=${profile}`, '--window-size=1500,1000', 'about:blank'],
  { windowsHide: true, stdio: 'ignore' });
const preview = process.env.TEAL_TEST_PREVIEW_DIR || path.join(process.cwd(), 'preview');
fs.mkdirSync(preview, { recursive: true });
const wait = ms => new Promise(resolve => setTimeout(resolve, ms));

function csvCount(file) {
  if (path.extname(file).toLowerCase() !== '.csv') return null;
  const s = fs.readFileSync(file, 'utf8').replace(/^\uFEFF/, '');
  let quoted = false, populated = false, rows = 0;
  for (let i = 0; i < s.length; i++) {
    const ch = s[i];
    if (ch === '"') {
      if (quoted && s[i + 1] === '"') i++;
      else quoted = !quoted;
      populated = true;
    } else if (ch === '\n' && !quoted) {
      if (populated) rows++;
      populated = false;
    } else if (ch !== '\r' || quoted) populated = true;
  }
  if (populated) rows++;
  return Math.max(0, rows - 1);
}

const expected = {
  ADSL: process.env.TEAL_TEST_ADSL_N === undefined ? csvCount(files.ADSL) : Number(process.env.TEAL_TEST_ADSL_N),
  ADAE: process.env.TEAL_TEST_ADAE_N === undefined ? csvCount(files.ADAE) : Number(process.env.TEAL_TEST_ADAE_N)
};

const aliases = {
  STUDYID: ['STUDYID', 'STUDY_ID', 'STUDY', 'PROTOCOL', 'PROTID', 'PROJECT'],
  USUBJID: ['USUBJID', 'SUBJECT_ID', 'SUBJECTID', 'SUBJECT', 'SUBJID', 'PATIENT_ID', 'PATIENTID', 'PATIENT', 'PAT', 'SUBJ', 'ID'],
  SITEID: ['SITEID', 'SITE_ID', 'CENTER_ID', 'CENTERID', 'CENTER', 'CENTRE', 'SITE', 'CEN', 'SITENO'],
  TRT01P: ['TRT01P', 'TRT01A', 'TRTA', 'TRTP', 'ARM', 'ARMCD', 'TREATMENT', 'TREATMENT_GROUP', 'TRT', 'GROUP'],
  AEBODSYS: ['AEBODSYS', 'AESOC', 'SOC', 'BODY_SYSTEM', 'BODSYS', 'BODYSYSTEM', 'SYSTEM_ORGAN_CLASS'],
  AEDECOD: ['AEDECOD', 'PT', 'AE_TERM', 'AETERM', 'EVENT', 'PREFERRED_TERM', 'TERM']
};

let ws;
async function main() {
  let port, target;
  for (let i = 0; i < 100; i++) {
    try {
      port = Number(fs.readFileSync(path.join(profile, 'DevToolsActivePort'), 'utf8').split(/\r?\n/)[0]);
      const targets = await (await fetch(`http://127.0.0.1:${port}/json/list`)).json();
      target = targets.find(t => t.type === 'page');
      if (target) break;
    } catch (_) {}
    await wait(250);
  }
  assert.ok(target, 'Edge CDP did not start');
  ws = new WebSocket(target.webSocketDebuggerUrl);
  await new Promise((resolve, reject) => {
    ws.addEventListener('open', resolve, { once: true });
    ws.addEventListener('error', reject, { once: true });
  });

  let seq = 0;
  const pending = new Map();
  ws.addEventListener('message', ev => {
    const msg = JSON.parse(ev.data);
    if (!pending.has(msg.id)) return;
    const { resolve, reject, timer } = pending.get(msg.id);
    clearTimeout(timer);
    pending.delete(msg.id);
    msg.error ? reject(new Error(JSON.stringify(msg.error))) : resolve(msg.result);
  });
  const cdp = (method, params = {}) => new Promise((resolve, reject) => {
    const id = ++seq;
    const timer = setTimeout(() => { pending.delete(id); reject(new Error(`CDP timeout: ${method}`)); }, 30000);
    pending.set(id, { resolve, reject, timer });
    ws.send(JSON.stringify({ id, method, params }));
  });
  const js = async expression => {
    const response = await cdp('Runtime.evaluate', { expression, returnByValue: true, awaitPromise: true });
    if (response.exceptionDetails) throw new Error(response.exceptionDetails.exception?.description || response.exceptionDetails.text);
    return response.result.value;
  };
  const body = () => js('document.body?.innerText || ""');
  const visible = id => js(`(() => { const el=document.getElementById(${JSON.stringify(id)}); return !!(el && el.getClientRects().length); })()`);
  const click = async id => assert.ok(await js(`(() => { const el=document.getElementById(${JSON.stringify(id)}); if(!el || el.disabled) return false; el.click(); return true; })()`), `Cannot click ${id}`);
  const settle = async (predicate, description, timeout = 45000) => {
    const started = Date.now();
    while (Date.now() - started < timeout) {
      try { if (await predicate()) return; } catch (_) {}
      await wait(250);
    }
    let summary = '';
    try { summary = (await body()).slice(0, 1600); } catch (_) {}
    throw new Error(`Timed out waiting for ${description}. Visible page: ${summary}`);
  };
  const shot = async name => {
    const result = await cdp('Page.captureScreenshot', { format: 'png' });
    fs.writeFileSync(path.join(preview, `${name}.png`), Buffer.from(result.data, 'base64'));
  };
  const waitIdle = () => settle(() => js('!document.documentElement.classList.contains("shiny-busy")'), 'Shiny idle');
  const waitUploadPage = () => settle(async () => (await js('!!document.getElementById("upload_ADSL")')) && (await visible('load_demo')) &&
    (await js('!!window.Shiny?.shinyapp?.$socket && window.Shiny.shinyapp.$socket.readyState===1')), 'upload form with a connected Shiny session');

  async function upload(domain) {
    const id = `upload_${domain}`;
    const { root } = await cdp('DOM.getDocument');
    const { nodeId } = await cdp('DOM.querySelector', { nodeId: root.nodeId, selector: `#${id}` });
    assert.ok(nodeId, `Missing file input ${id}`);
    await cdp('DOM.setFileInputFiles', { nodeId, files: [files[domain]] });
    const mapId = `map_${domain}_USUBJID`;
    await settle(() => js(`(() => {const el=document.getElementById(${JSON.stringify(mapId)}); return !!el && ((el.selectize && Object.keys(el.selectize.options).length>1) || el.options?.length>1);})()`), `${domain} column mapping`);
    await waitIdle();
  }

  async function selectMapping(domain, field, required = true) {
    const id = `map_${domain}_${field}`;
    const state = await js(`(() => { const el=document.getElementById(${JSON.stringify(id)}); if(!el) return null;
      return { value: el.selectize ? el.selectize.getValue() : el.value,
        choices: el.selectize ? Object.keys(el.selectize.options) : [...el.options].map(o=>o.value) }; })()`);
    if (!state) { assert.ok(!required, `Missing mapping selector ${id}`); return; }
    const wanted = mappings[domain]?.[field];
    const known = aliases[field] || [field];
    const normalized = s => s.toUpperCase().replace(/[^A-Z0-9]/g, '');
    const found = wanted || known.map(a => state.choices.find(c => normalized(c) === normalized(a))).find(Boolean);
    if (!found) {
      assert.ok(!required || state.value, `No source field for required ${id}; options=${JSON.stringify(state.choices)}. Set TEAL_TEST_MAP.`);
      console.log('MAP', domain, field, state.value || '(not selected)');
      return;
    }
    assert.ok(state.choices.includes(found), `Requested source ${found} not available in ${id}`);
    if (state.value !== found) {
      await js(`(() => { const el=document.getElementById(${JSON.stringify(id)}); const val=${JSON.stringify(found)};
        if(el.selectize) el.selectize.setValue(val); else {el.value=val;el.dispatchEvent(new Event('change',{bubbles:true}));} })()`);
      await settle(() => js(`(() => {const el=document.getElementById(${JSON.stringify(id)}); return (el.selectize?el.selectize.getValue():el.value)===${JSON.stringify(found)};})()`), `${id} selection`);
    }
    console.log('MAP', domain, field, found, state.value === found ? '(auto)' : '(selected)');
  }

  const menu = () => js(`[...document.querySelectorAll('a.module-button')].map(a=>a.textContent.trim())`);
  const assertModule = async name => assert.ok((await menu()).includes(name), `Missing teal module ${name}`);
  const showModule = async name => {
    assert.ok(await js(`(() => { const el=[...document.querySelectorAll('a.module-button')].find(a=>a.textContent.trim()===${JSON.stringify(name)}); if(!el)return false;el.click();return true;})()`), `Cannot open module ${name}`);
    await wait(700);
    await settle(() => js(`!!document.querySelector('.tab-pane.teal_module.active')`), `active teal module ${name}`);
    await waitIdle();
  };
  const assertNoErrors = async () => {
    const errors = await js(`[...document.querySelectorAll('.shiny-output-error')].filter(x=>x.getClientRects().length).map(x=>x.innerText.trim()).filter(Boolean)`);
    assert.deepEqual(errors, [], 'Visible Shiny output errors');
  };
  const countMatches = async (domain, n) => {
    if (n === null) return true;
    const escaped = String(n).replace(/[.*+?^$\{\}()|\[\]\\]/g, '\\$&');
    const formatted = String(n).replace(/\B(?=(\d{3})+(?!\d))/g, ',');
    const text = await body();
    const pattern = `(?:^|\\s)${domain}\\s+(?:${escaped}|${formatted})(?![0-9])\\s*(?:/\\s*(?:${escaped}|${formatted}))?`;
    return new RegExp(pattern, 'm').test(text);
  };

  await cdp('Page.enable'); await cdp('Runtime.enable'); await cdp('DOM.enable');
  await cdp('Page.navigate', { url });
  await waitUploadPage();
  await upload('ADSL');
  for (const field of ['STUDYID', 'USUBJID', 'SITEID', 'TRT01P']) await selectMapping('ADSL', field, field !== 'STUDYID');
  await upload('ADAE');
  for (const field of ['USUBJID', 'AEBODSYS', 'AEDECOD']) await selectMapping('ADAE', field);
  await shot('upload_01_mapping');
  await click('start_analysis');
  await settle(async () => (await menu()).includes('研究总览'), 'native teal module menu');
  await showModule('研究总览');
  for (const label of ['数据明细', 'AE SOC/PT 统计表', 'AE 图形（SOC/PT）']) await assertModule(label);
  await settle(() => countMatches('ADSL', expected.ADSL), `ADSL count ${expected.ADSL}`);
  await assertNoErrors();
  console.log('UPLOAD OVERVIEW', JSON.stringify({ expected, menu: await menu() }));
  await shot('upload_02_overview');
  await showModule('AE SOC/PT 统计表');
  await settle(() => countMatches('ADAE', expected.ADAE), `ADAE count ${expected.ADAE}`);
  await assertNoErrors();
  await shot('upload_03_ae_table');
  await showModule('AE 图形（SOC/PT）');
  await assertNoErrors();
  await shot('upload_04_ae_graph');
  await click('change_data');
  await waitUploadPage();
  assert.equal((await menu()).length, 0, 'Old teal navigation remained on data-selection page');
  await click('load_demo');
  await settle(async () => (await menu()).includes('研究总览'), 'demo native teal module menu');
  await showModule('研究总览');
  await settle(() => countMatches('ADSL', 360), 'demo ADSL count 360');
  await assertModule('KRI 风险矩阵');
  await assertModule('连续指标访视趋势');
  await assertNoErrors();
  await shot('upload_05_demo');
  console.log('PASS: browser file upload, mapping, native teal modules, data counts, AE pages, change data, and demo');
  await cdp('Browser.close').catch(() => {});
}

main().catch(error => { console.error(error); process.exitCode = 1; }).finally(() => {
  try { ws?.close(); } catch (_) {}
  try { browser.kill(); } catch (_) {}
});
