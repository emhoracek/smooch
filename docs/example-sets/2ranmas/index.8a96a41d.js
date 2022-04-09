function t(t,e){const s=h(t[0],e);if(s)return s.altmap.bind(s)}function e(t,e){return e.changeSet.bind(e,t[0])}function s(t,e){const s=h(t[0],e);if(s)return s.map.bind(s)}function n(t,e){const s=h(t[0],e);if(s){const e=t[1],n=t[2];return s.move.bind(s,e,n)}}function i(t,e){e.logger.debug("Nothing happened :)")}function o(t,e){const s=t[0].replace(".au",".wav"),n=e.getSound(s);return()=>n.play()}function r(t,e){const s=t[0],n=t[1];return e.setTimer.bind(e,s,n)}function a(t,e){const s=h(t[0],e);if(s){const e=t[1];return s.setTransparency.bind(s,e)}}function c(t,e){const s=h(t[0],e);if(s)return s.unmap.bind(s)}function h(t,e){if("number"==typeof t){const s=e.getObject(t);if(s)return s;e.logger.warn(`Unable to find object with mark "#${t}"`)}if(t.endsWith(".cel")){const s=e.getCel(t);if(s)return s;e.logger.warn(`Unable to find cel with filename "${t}"`)}e.logger.error(`Expected a cel or object reference but got "${t}"`)}function l(h,l){const u=d[h.event];if(u){const d=h.actions.map((h=>function(h,l){const d={altmap:t,changeset:e,map:s,move:n,nop:i,sound:o,transparent:a,timer:r,unmap:c}[h.action];if(d)return d(h.args,l);l.logger.warn(`Unknown action "${h.action}"`)}(h,l))).filter((t=>void 0!==t));u(h.args,d,l)}else l.logger.warn(`Unknown event "${h.event}"`)}const d={alarm:function(t,e,s){const n=t[0];s.timers[n]={timeout:!1,callback:()=>{e.forEach((t=>t())),s.update(),s.draw()}}},begin:function(t,e,s){s.addEventListener("begin",(t=>e.forEach((t=>t()))))},catch:function(t,e,s){h(t[0],s).addEventListener("catch",(t=>e.forEach((t=>t()))))},col:function(t,e,s){s.logger.warn("Not implemented: `col`. Smooch doesn't implement palette groups.")},drop:function(t,e,s){h(t[0],s).addEventListener("drop",(t=>e.forEach((t=>t()))))},end:function(t,e,s){s.logger.warn("Not implemented: `end`. The browser environment makes this event impractical.")},fixcatch:function(t,e,s){h(t[0],s).addEventListener("fixcatch",(t=>e.forEach((t=>t()))))},fixdrop:function(t,e,s){h(t[0],s).addEventListener("fixdrop",(t=>e.forEach((t=>t()))))},initialize:function(t,e,s){s.addEventListener("initialize",(t=>e.forEach((t=>t()))))},never:function(){},press:function(t,e,s){const n=h(t[0],s);n&&n.addEventListener("press",(t=>e.forEach((t=>{t&&t(),s.update(),s.draw()}))))},release:function(t,e,s){h(t[0],s).addEventListener("release",(t=>e.forEach((t=>t()))))},set:function(t,e,s){const n=t[0];s.addEventListener(`set-${n}`,(t=>e.forEach((t=>t()))))},unfix:function(t,e,s){s.logger.warn("Not implemented: `unfix`. Smooch doesn't (currently) decrement fix.")}};class u extends EventTarget{constructor(t,e,s){return super(),this.name=e.name,this.mark=e.mark,this.id=e.name,this.index=s,this.fix=e.fix,this.position=t.positions[0],this.positions=t.positions,this.sets=e.sets,this.palette=e.palette,this.image=!1,this.ghostImage=void 0,this.visible=!1,this.mapped=!0,this.alpha=e.alpha,this.currentSet=0,this.offset=e.offset,this}loadImage(t,e){const s=t.ctxt,n=t.canvas,i=`/sets/${t.staticDirectory}/palette${this.palette}/${this.name}.png`,o=this,r=new Image;r.addEventListener("load",(i=>{s.drawImage(r,0,0,r.width,r.height);const a=s.getImageData(0,0,r.width,r.height),c=a.data,h={red:(16711680&(l=o.index))>>16,green:(65280&l)>>8,blue:255&l};var l;for(let t=0;t<c.length;t+=4)c[t]=h.red,c[t+1]=h.green,c[t+2]=h.blue;s.clearRect(0,0,t.size.x,t.size.y),s.putImageData(a,0,0),o.ghostImage=new Image,o.ghostImage.src=n.toDataURL("image/png"),o.ghostImage.addEventListener("load",(t=>{e()})),o.image=r})),r.src=i}update(t){this.currentSet=t,-1!==this.sets.indexOf(t)&&this.mapped?this.visible=!0:this.visible=!1,this.position=this.positions[t]}draw(t,e){if(!this.image)return!1;!0===this.visible&&(this.alpha&&(t.globalAlpha=(255-this.alpha)/255),t.drawImage(this.image,this.position.x+this.offset.x,this.position.y+this.offset.y),t.globalAlpha=1,e.drawImage(this.ghostImage,this.position.x+this.offset.x,this.position.y+this.offset.y))}altmap(){this.mapped=!this.mapped}map(){this.mapped=!0}move(t,e){this.positions[this.currentSet].x+=t,this.positions[this.currentSet].y+=e}setTransparency(t){this.alpha=t}unmap(){this.mapped=!1}}class g extends EventTarget{constructor(t,e){return super(),this.mark=t,this.id=t,this.currentSet=0,this.positions=e,this.cels=[],this}update(t){this.currentSet=t;for(let e=0;e<this.cels.length;e++)this.cels[e].update(t)}get position(){return this.positions[this.currentSet]}get fixed(){return this.cels.some((t=>t.fix>0))}setPosition(t,e){this.positions[this.currentSet].x=t,this.positions[this.currentSet].y=e}altmap(){this.cels.forEach((t=>{t.altmap()}))}map(){this.cels.forEach((t=>t.map()))}move(t,e){this.positions[this.currentSet].x+=t,this.positions[this.currentSet].y+=e}setTransparency(t){this.cels.forEach((e=>e.setTransparency(t)))}unmap(){this.cels.forEach((t=>t.unmap()))}}class m{constructor(t){["debug","warning","error"].includes(t)?this.logLevel=t:this.logLevel="none"}debug(t){"none"!==this.logLevel&&console.log(t)}warn(t){"none"!==this.logLevel&&"debug"!==this.logLevel&&console.log(t)}error(t){"error"===this.logLevel&&console.log(t)}}class p extends EventTarget{constructor(t,e){return super(),this.staticDirectory=e,this.cnf=t,this.logger=new m("debug"),this.size={x:t.window_size[0],y:t.window_size[1]},this.borderColor=t.border,this.backgroundColor=t.background,this.currentSet=0,this.objs=[],this.cels=[],this.timers=[],this}initialize(t){document.getElementById("borderarea").style.background=this.borderColor;const e=document.getElementById("playarea");e.style.width=this.size.x+"px",e.style.height=this.size.y+"px",e.style.background=this.backgroundColor,this.initCanvases(this.size),this.initCels(this.cnf.cels,this.cnf.positions),function(t){const e=document.getElementsByClassName("set");for(let s=0;s<e.length;s++)e[s].addEventListener("click",(function(){const e=parseInt(this.innerHTML);t.changeSet(e)}));const s=document.getElementsByClassName("tip")[0];document.getElementById("open-tip").addEventListener("click",(t=>{s.style.display="block"}));document.getElementById("close-tip").addEventListener("click",(t=>{s.style.display="none"}))}(this),this.cnf.fkiss&&this.initFKiSS(this.cnf.fkiss),this.dispatchEvent(new CustomEvent("initialize")),this.cels.forEach((e=>e.loadImage(this,t)))}begin(){this.update(),this.draw(),this.dispatchEvent(new CustomEvent("begin"))}initCels(t,e,s){t.reverse(),t.forEach(((t,s)=>{const n=this.objs[t.mark];if(n){const e=new u(n,t,s);n.cels.push(e),this.cels.push(e)}else{const n=new g(t.mark,e.map((e=>e.positions[t.mark]||{x:0,y:0}))),i=new u(n,t,s);n.cels.push(i),this.objs[t.mark]=n,this.cels.push(i)}}))}initCanvases(t){const e=document.getElementById("screen");f(e,t);const s=document.getElementById("ghost");f(s,t),this.canvas=e,this.ctxt=e.getContext("2d"),this.ghost=s.getContext("2d"),e.addEventListener("touchstart",(function(t){t.preventDefault()})),e.addEventListener("touchmove",(function(t){t.preventDefault()})),e.addEventListener("touchend",(function(t){t.preventDefault()})),e.addEventListener("touchcancel",(function(t){t.preventDefault()}))}changeSet(t){this.update(t),this.draw(),function(t){const e=document.getElementsByClassName("set");for(let s=0;s<e.length;s++)t===parseInt(e[s].innerHTML)?e[s].style.color="black":e[s].style.color="grey"}(this.currentSet)}getSelectedObject(t){const e=this.ghost.getImageData(t.x,t.y,1,1).data,s=(n=e[0],i=e[1],o=e[2],(n<<16)+(i<<8)+o);var n,i,o;if(0===e[3])return console.log("not draggable"),!1;{const t=this.cels[s];return{object:this.objs[t.mark],cel:t}}}getObjectPosition(t){return t.positions[this.currentSet]}getCel(t){return this.cels.find((e=>e.name+".cel"===t))}getObject(t){return this.objs[t]}getSound(t){return document.getElementById(t.replace(".wav",""))}setTimer(t,e){const s=this.timers[t],n=!!s&&s.timeout,i=s?s.callback:()=>{};n&&clearTimeout(n);const o=setTimeout((()=>{this.timers[t].timeout=!1,this.timers[t].callback()}),e);this.timers[t]={timeout:o,callback:i}}moveObject(t,e,s){t.setPosition(e,s),this.update(),this.draw()}initFKiSS(t){t.forEach((t=>{l(t,this)}))}update(t){void 0!==t&&(this.currentSet=t);for(let t=0;t<this.objs.length;t++)this.objs[t]&&this.objs[t].update(this.currentSet)}draw(){this.ctxt.clearRect(0,0,this.size.x,this.size.y),this.ghost.clearRect(0,0,this.size.x,this.size.y),this.cels.forEach((t=>t.draw(this.ctxt,this.ghost)))}}function f(t,e){t.style.width=e.x+"px",t.style.height=e.y+"px",t.width=e.x,t.height=e.y}class v{constructor(t){this.dragHandler=!1,this.doll=t,this.screen=document.getElementById("screen");const e=document.getElementById("ghost");this.ctxt=e.getContext("2d")}initialize(){document.addEventListener("pointerdown",(t=>this.onMouseDown(t))),document.addEventListener("pointerup",(()=>{this.dragHandler&&(document.removeEventListener("pointermove",this.dragHandler),this.dragHandler=!1)}))}onMouseDown(t){const e=this.getMousePos(t),s=this.doll.getSelectedObject(e);if(s&&(s.object.dispatchEvent(b),s.cel.dispatchEvent(b),!s.object.fixed)){const n=this.getDragStart(s.object,e);this.dragHandler=t=>this.onMouseMove(t,s.object,n),document.addEventListener("pointermove",this.dragHandler),t.preventDefault()}}getDragStart(t,e){const s=this.doll.getObjectPosition(t);return{x:s.x-e.x,y:s.y-e.y}}onMouseMove(t,e,s){const n=this.getMousePos(t),i=s.x+n.x,o=s.y+n.y;this.doll.moveObject(e,i,o),t.preventDefault()}getMousePos(t){const e=this.screen.getBoundingClientRect();return{x:t.clientX-e.left,y:t.clientY-e.top}}}const b=new CustomEvent("press");window.addEventListener("load",(function(){let t=!1,e=0;const s=document.getElementById("set-data").dataset.staticDirectory;fetch(`/sets/${s}/setdata.json`).then((t=>t.json())).then((n=>{const i=n.cels.length;window.setTimeout((function s(){console.log(`loading ${e} of ${i}`),e<i?window.setTimeout(s,500):(t.begin(),document.getElementById("loading").style.display="none",document.getElementById("sets").style.display="block",document.getElementById("borderarea").style.display="block")}),500),t=new p(n,s);const o=new v(t);t.initialize((()=>{e+=1})),o.initialize(),"undefined"!=typeof globalKiss&&(globalKiss=t)}))}));
//# sourceMappingURL=index.8a96a41d.js.map
