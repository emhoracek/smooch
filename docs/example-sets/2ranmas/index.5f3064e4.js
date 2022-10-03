const t={altmap:function(t,n){const s=e(t[0],n);if(s)return s.altmap.bind(s)},changeset:function(t,e){return e.changeSet.bind(e,t[0])},map:function(t,n){const s=e(t[0],n);if(s)return s.map.bind(s)},move:function(t,n){const s=e(t[0],n);if(s){const e=t[1],n=t[2];return s.move.bind(s,e,n)}},nop:function(t,e){return()=>e.logger.debug("Nothing happened :)")},sound:function(t,e){const n=t[0],s=e.getOrCreateSound(n);return s.play.bind(s)},transparent:function(t,n){const s=e(t[0],n);if(s){const e=t[1];return s.setTransparency.bind(s,e)}},timer:function(t,e){const n=t[0],s=t[1];return e.setTimer.bind(e,n,s)},unmap:function(t,n){const s=e(t[0],n);if(s)return s.unmap.bind(s)}};function e(t,e){if("number"==typeof t){const n=e.getObject(t);if(n)return n;e.logger.warn(`Unable to find object with mark "#${t}"`)}if(t.endsWith(".cel")){const n=e.getCel(t);if(n)return n;e.logger.warn(`Unable to find cel with filename "${t}"`)}e.logger.error(`Expected a cel or object reference but got "${t}"`)}function n(e,n){const i=s[e.event];if(i){const s=e.actions.map((e=>function(e,n){const s=t[e.action];if(s)return s(e.args,n);n.logger.warn(`Unknown action "${e.action}"`)}(e,n))).filter((t=>void 0!==t));i(e.args,s,n)}else n.logger.warn(`Unknown event "${e.event}"`)}const s={alarm:function(t,e,n){const s=t[0];n.timers[s]={timeout:!1,callback:()=>{e.forEach((t=>t())),n.update(),n.draw()}}},begin:function(t,e,n){n.addEventListener("begin",(t=>{e.forEach((t=>t())),n.update(),n.draw()}))},catch:function(t,n,s){e(t[0],s).addEventListener("catch",(t=>n.forEach((t=>t()))))},col:function(t,e,n){n.logger.warn("Not implemented: `col`. Smooch doesn't implement palette groups.")},drop:function(t,n,s){e(t[0],s).addEventListener("drop",(t=>n.forEach((t=>t()))))},end:function(t,e,n){n.logger.warn("Not implemented: `end`. The browser environment makes this event impractical.")},fixcatch:function(t,n,s){e(t[0],s).addEventListener("fixcatch",(t=>n.forEach((t=>t()))))},fixdrop:function(t,n,s){e(t[0],s).addEventListener("fixdrop",(t=>n.forEach((t=>t()))))},initialize:function(t,e,n){n.addEventListener("initialize",(t=>e.forEach((t=>t()))))},never:function(){},press:function(t,n,s){const i=e(t[0],s);i&&i.addEventListener("press",(t=>n.forEach((t=>{t&&t(),s.update(),s.draw()}))))},release:function(t,n,s){e(t[0],s).addEventListener("release",(t=>n.forEach((t=>{t&&t(),s.update(),s.draw()}))))},set:function(t,e,n){const s=t[0];n.addEventListener(`set-${s}`,(t=>e.forEach((t=>t()))))},unfix:function(t,e,n){n.logger.warn("Not implemented: `unfix`. Smooch doesn't (currently) decrement fix.")}};class i extends EventTarget{constructor(t,e,n){return super(),this.name=e.name,this.mark=e.mark,this.id=e.name,this.index=n,this.fix=e.fix,this.position=t.positions[0],this.positions=t.positions,this.sets=e.sets,this.palette=e.palette,this.image=!1,this.ghostImage=void 0,this.visible=!1,this.mapped=!0,this.alpha=e.alpha,this.currentSet=0,this.offset=e.offset,this}loadImage(t,e){const n=t.ctxt,s=t.canvas,i=`${t.staticDirectory}/palette${this.palette}/${this.name}.png`,o=this,a=new Image;a.addEventListener("load",(i=>{n.clearRect(0,0,t.size.x,t.size.y),n.drawImage(a,0,0,a.width,a.height);const r=n.getImageData(0,0,a.width,a.height),c=r.data,l={red:(16711680&(h=o.index))>>16,green:(65280&h)>>8,blue:255&h};var h;for(let t=0;t<c.length;t+=4)c[t]=l.red,c[t+1]=l.green,c[t+2]=l.blue;n.clearRect(0,0,t.size.x,t.size.y),n.putImageData(r,0,0),o.ghostImage=new Image,o.ghostImage.src=s.toDataURL("image/png"),o.ghostImage.addEventListener("load",(t=>{e()})),o.image=a})),a.src=i}update(t){this.currentSet=t,-1!==this.sets.indexOf(t)&&this.mapped?this.visible=!0:this.visible=!1,this.position=this.positions[t]}draw(t,e){if(!this.image)return!1;!0===this.visible&&(this.alpha&&(t.globalAlpha=(255-this.alpha)/255),t.drawImage(this.image,this.position.x+this.offset.x,this.position.y+this.offset.y),t.globalAlpha=1,e.drawImage(this.ghostImage,this.position.x+this.offset.x,this.position.y+this.offset.y))}altmap(){this.mapped=!this.mapped}map(){this.mapped=!0}move(t,e){this.positions[this.currentSet].x+=t,this.positions[this.currentSet].y+=e}setTransparency(t){this.alpha=t}unmap(){this.mapped=!1}}class o{constructor(t,e){const n=`${t}/${e.replace(".au",".wav")}`;return this.audio=new Audio(n),this}play(){if(this.audio.HAVE_ENOUGH_DATA)return this.audio.play();this.audio.addEventListener("canplaythrough",(t=>this.audio.play()))}}class a extends EventTarget{constructor(t,e){return super(),this.mark=t,this.id=t,this.currentSet=0,this.positions=e,this.cels=[],this}update(t){this.currentSet=t;for(let e=0;e<this.cels.length;e++)this.cels[e].update(t)}get position(){return this.positions[this.currentSet]}get fixed(){return this.cels.some((t=>t.fix>0))}setPosition(t,e){this.positions[this.currentSet].x=t,this.positions[this.currentSet].y=e}altmap(){this.cels.forEach((t=>{t.altmap()}))}map(){this.cels.forEach((t=>t.map()))}move(t,e){this.positions[this.currentSet].x+=t,this.positions[this.currentSet].y+=e}setTransparency(t){this.cels.forEach((e=>e.setTransparency(t)))}unmap(){this.cels.forEach((t=>t.unmap()))}}class r{constructor(t){["debug","warning","error"].includes(t)?this.logLevel=t:this.logLevel="none"}debug(t){"none"!==this.logLevel&&console.log(t)}warn(t){"none"!==this.logLevel&&"debug"!==this.logLevel&&console.log(t)}error(t){"error"===this.logLevel&&console.log(t)}}class c extends EventTarget{constructor(t,e){return super(),this.staticDirectory=e,this.cnf=t,this.logger=new r("debug"),this.size={x:t.window_size[0],y:t.window_size[1]},this.borderColor=t.border,this.backgroundColor=t.background,this.currentSet=0,this.objs=[],this.cels=[],this.sounds=[],this.timers=[],this}initialize(t){document.getElementById("borderarea").style.background=this.borderColor;const e=document.getElementById("playarea");e.style.width=this.size.x+"px",e.style.height=this.size.y+"px",e.style.background=this.backgroundColor,this.initCanvases(this.size),this.initCels(this.cnf.cels,this.cnf.positions),function(t){const e=document.getElementsByClassName("set");for(let n=0;n<e.length;n++)e[n].addEventListener("click",(function(){const e=parseInt(this.innerHTML);t.changeSet(e)}));const n=document.getElementsByClassName("tip")[0];document.getElementById("open-tip").addEventListener("click",(t=>{n.style.display="block"}));document.getElementById("close-tip").addEventListener("click",(t=>{n.style.display="none"}))}(this),this.cnf.fkiss&&this.initFKiSS(this.cnf.fkiss),this.dispatchEvent(new CustomEvent("initialize")),this.cels.forEach((e=>e.loadImage(this,t)))}begin(){this.update(),this.draw(),this.dispatchEvent(new CustomEvent("begin"))}initCels(t,e,n){t.reverse(),t.forEach(((t,n)=>{const s=this.objs[t.mark];if(s){const e=new i(s,t,n);s.cels.push(e),this.cels.push(e)}else{const s=new a(t.mark,e.map((e=>e.positions[t.mark]||{x:0,y:0}))),o=new i(s,t,n);s.cels.push(o),this.objs[t.mark]=s,this.cels.push(o)}}))}initCanvases(t){const e=document.getElementById("screen");l(e,t);const n=document.getElementById("ghost");l(n,t),this.canvas=e,this.ctxt=e.getContext("2d"),this.ghost=n.getContext("2d"),e.addEventListener("touchstart",(function(t){t.preventDefault()})),e.addEventListener("touchmove",(function(t){t.preventDefault()})),e.addEventListener("touchend",(function(t){t.preventDefault()})),e.addEventListener("touchcancel",(function(t){t.preventDefault()}))}changeSet(t){this.update(t),this.draw(),function(t){const e=document.getElementsByClassName("set");for(let n=0;n<e.length;n++)t===parseInt(e[n].innerHTML)?e[n].style.color="black":e[n].style.color="grey"}(this.currentSet)}getSelectedObject(t){const e=this.ghost.getImageData(t.x,t.y,1,1).data,n=(s=e[0],i=e[1],o=e[2],(s<<16)+(i<<8)+o);var s,i,o;if(0===e[3])return console.log("not draggable"),!1;{const t=this.cels[n];return{object:this.objs[t.mark],cel:t}}}getObjectPosition(t){return t.positions[this.currentSet]}getCel(t){return this.cels.find((e=>e.name+".cel"===t))}getObject(t){return this.objs[t]}createSound(t){this.sounds[t]=new o(t)}getOrCreateSound(t){const e=this.sounds[t];if(e)return e;{const e=new o(this.staticDirectory,t);return this.sounds[t]=e,e}}setTimer(t,e){const n=this.timers[t],s=!!n&&n.timeout,i=n?n.callback:()=>{};s&&clearTimeout(s);const o=setTimeout((()=>{this.timers[t].timeout=!1,this.timers[t].callback()}),e);this.timers[t]={timeout:o,callback:i}}moveObject(t,e,n){t.setPosition(e,n),this.update(),this.draw()}initFKiSS(t){t.forEach((t=>{n(t,this)}))}update(t){void 0!==t&&(this.currentSet=t);for(let t=0;t<this.objs.length;t++)this.objs[t]&&this.objs[t].update(this.currentSet)}draw(){this.ctxt.clearRect(0,0,this.size.x,this.size.y),this.ghost.clearRect(0,0,this.size.x,this.size.y),this.cels.forEach((t=>t.draw(this.ctxt,this.ghost)))}}function l(t,e){t.style.width=e.x+"px",t.style.height=e.y+"px",t.width=e.x,t.height=e.y}class h{constructor(t){this.dragHandler=!1,this.releaseHandler=!1,this.doll=t,this.screen=document.getElementById("screen");const e=document.getElementById("ghost");this.ctxt=e.getContext("2d")}initialize(){document.addEventListener("pointerdown",(t=>{this.releaseHandler&&(document.removeEventListener("pointerup",this.releaseHandler),this.releaseHandler=!1),this.onMouseDown(t)}))}onRelease(t,e){e&&(e.object.dispatchEvent(u),e.cel.dispatchEvent(u)),this.dragHandler&&(document.removeEventListener("pointermove",this.dragHandler),this.dragHandler=!1)}onMouseDown(t){const e=this.getMousePos(t),n=this.doll.getSelectedObject(e);if(n&&(n.object.dispatchEvent(d),n.cel.dispatchEvent(d),this.releaseHandler=t=>this.onRelease(t,n),document.addEventListener("pointerup",this.releaseHandler),!n.object.fixed)){const s=this.getDragStart(n.object,e);this.dragHandler=t=>this.onMouseMove(t,n.object,s),document.addEventListener("pointermove",this.dragHandler),t.preventDefault()}}getDragStart(t,e){const n=this.doll.getObjectPosition(t);return{x:n.x-e.x,y:n.y-e.y}}onMouseMove(t,e,n){const s=this.getMousePos(t),i=n.x+s.x,o=n.y+s.y;this.doll.moveObject(e,i,o),t.preventDefault()}getMousePos(t){const e=this.screen.getBoundingClientRect();return{x:t.clientX-e.left,y:t.clientY-e.top}}}const d=new CustomEvent("press"),u=new CustomEvent("release");window.addEventListener("load",(function(){let t=!1,e=0;const n=document.getElementById("set-data").dataset.staticDirectory;fetch(`${n}/setdata.json`).then((t=>t.json())).then((s=>{const i=s.cels.length;window.setTimeout((function n(){console.log(`loading ${e} of ${i}`),e<i?window.setTimeout(n,500):(t.begin(),document.getElementById("loading").style.display="none",document.getElementById("sets").style.display="flex",document.getElementById("borderarea").style.display="block")}),500),t=new c(s,n);const o=new h(t);t.initialize((()=>{e+=1})),o.initialize(),function(){const t=document.getElementById("documents-button"),e=document.getElementById("documents");t.addEventListener("click",(t=>{e.classList.toggle("show-docs")}));const n=document.getElementsByClassName("view-file"),s=document.getElementById("notepad-modal"),i=document.querySelector("#notepad-modal pre"),o=document.getElementById("notepad-title");for(let t=0;t<n.length;t++)n[t].addEventListener("click",(t=>{const e=t.target.href,n=t.target.dataset.filename;t.preventDefault(),fetch(e).then((t=>t.text())).then((t=>{s.style.display="block",i.style.display="block",i.textContent=t,o.textContent=n,i.scrollTop=0}))}));document.getElementById("notepad-close").addEventListener("click",(t=>{s.style.display="none",t.preventDefault()}))}(),"undefined"!=typeof globalKiss&&(globalKiss=t)}))}));
//# sourceMappingURL=index.5f3064e4.js.map
