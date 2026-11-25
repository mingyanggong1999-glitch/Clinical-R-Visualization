// Local Shiny UI smoke test using a temporary Edge profile and Chromium CDP.
// Run: node tests/browser_check.js http://127.0.0.1:18765
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
const {spawn} = require('node:child_process');
const url = process.argv[2] || 'http://127.0.0.1:18765';
const edge = 'C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe';
const profile = fs.mkdtempSync(path.join(os.tmpdir(), 'teal-browser-'));
const browser = spawn(edge, ['--headless=new', '--disable-gpu', '--no-first-run', '--no-default-browser-check',
  '--remote-debugging-port=19876', `--user-data-dir=${profile}`, '--window-size=1500,1000', 'about:blank'],
  {windowsHide:true, stdio:'ignore'});
const wait = ms => new Promise(r => setTimeout(r, ms));
const preview = path.join(process.cwd(), 'preview');
fs.mkdirSync(preview, {recursive:true});

(async () => {
  let targets;
  for (let i=0; i<50; i++) {
    try { targets = await (await fetch('http://127.0.0.1:19876/json/list')).json(); if(targets?.length) break; }
    catch (_) {}
    await wait(250);
  }
  if (!targets?.length) throw new Error('Edge CDP did not start');
  const ws = new WebSocket(targets.find(t => t.type==='page').webSocketDebuggerUrl);
  await new Promise((resolve,reject)=>{ws.addEventListener('open',resolve,{once:true});ws.addEventListener('error',reject,{once:true})});
  let seq=0; const pending=new Map();
  ws.addEventListener('message',ev=>{const m=JSON.parse(ev.data); if(pending.has(m.id)){const {resolve,reject}=pending.get(m.id);pending.delete(m.id);m.error?reject(m.error):resolve(m.result)}});
  const cdp=(method,params={})=>new Promise((resolve,reject)=>{const id=++seq;pending.set(id,{resolve,reject});ws.send(JSON.stringify({id,method,params}))});
  const js=async expression=>(await cdp('Runtime.evaluate',{expression,returnByValue:true,awaitPromise:true})).result.value;
  const shot=async name=>{const result=await cdp('Page.captureScreenshot',{format:'png'});fs.writeFileSync(path.join(preview,name+'.png'),Buffer.from(result.data,'base64'))};
  await cdp('Page.enable'); await cdp('Runtime.enable');
  await cdp('Page.navigate',{url}); await wait(10000);
  const inspect = async () => await js(`(() => ({title:document.title,ready:!!window.Shiny,body:document.body.innerText.slice(0,1800),errors:[...document.querySelectorAll('.shiny-output-error')].filter(x=>x.getClientRects().length).map(x=>x.innerText),nav:[...document.querySelectorAll('a,button')].map(x=>x.innerText.trim()).filter(Boolean).slice(0,100),plots:[...document.querySelectorAll('.plotly, .shiny-plot-output, .datatables')].filter(x=>x.getClientRects().length).length}))()`);
  console.log('HOME', JSON.stringify(await inspect())); await shot('01_overview');
  const clickText=async text=>await js(`(() => {const x=[...document.querySelectorAll('a.module-button')].find(el=>el.textContent.trim()===${JSON.stringify(text)});if(x){x.click();return true}return false})()`);
  const panels=[['风险监察','KRI 风险矩阵','02_kri'],['风险监察','Query 追踪','03_query'],['风险监察','入组进度','04_enrollment'],['临床统计','人口学与基线','05_demog'],['临床统计','AE SOC/PT 统计表','06_ae_table'],['临床统计','AE 图形（SOC/PT）','07_ae_graph'],['临床统计','连续指标访视趋势','08_longitudinal'],['临床统计','KM 生存曲线','09_km'],['通用探索','直方图 / 分布','10_distribution'],['通用探索','箱线图 / 双变量关系','11_bivariate'],['通用探索','散点图','12_scatter'],['通用探索','数据明细','13_data'],['通用探索','变量描述','14_variables'],['通用探索','缺失值分布','15_missing']];
  for (const [group,panel,name] of panels) {
    if(process.env.TEAL_TEST_PANELS && !process.env.TEAL_TEST_PANELS.split(',').includes(name)) continue;
    if(!await clickText(panel)) {console.log('NOT FOUND',panel);continue;}
    await wait(3000);
    for(let i=0;i<44;i++) {
      const busy=await js(`document.body.innerText.includes('Computing ...') || document.documentElement.classList.contains('shiny-busy')`);
      if(!busy) break;
      await wait(500);
    }
    await wait(1000);
    if(panel==='AE 图形（SOC/PT）') {await js(`document.querySelector('.tab-pane.teal_module.active table tbody tr')?.click()`);await wait(1200);}
    const state=await inspect(); const active=await js(`document.querySelector('.tab-pane.teal_module.active')?.id`);
    console.log('PANEL',panel,JSON.stringify({active,errors:state.errors,plots:state.plots,body:state.body.slice(0,1200)})); await shot(name);
  }
  ws.close();
})().catch(err=>{console.error(err);process.exitCode=1}).finally(()=>{browser.kill()});

