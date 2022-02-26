(()=>{class t{constructor(t,e,s,i){return this.color=t.color,this.name=e.name,this.mark=e.mark,this.fix=e.fix,this.position=t.positions[0],this.positions=t.positions,this.sets=e.sets,this.image=document.getElementById(e.name+"-"+e.palette),this.ghostImage=void 0,this.visible=!1,this.alpha=e.alpha,this.offset=e.offset,this.init(s),this.ghostImage.onload=function(){i()},this}init(t){const e=t.ctxt,s=t.canvas,i=this.image;e.drawImage(i,0,0,i.width,i.height);const n=e.getImageData(0,0,i.width,i.height),o=n.data,h=this.color;for(let t=0;t<o.length;t+=4)o[t]=h.red,o[t+1]=h.green,o[t+2]=h.blue;e.clearRect(0,0,t.size.x,t.size.y),e.putImageData(n,0,0),this.ghostImage=new Image,this.ghostImage.src=s.toDataURL("image/png"),e.clearRect(0,0,t.size.x,t.size.y)}update(t){this.currentSet=t,-1===this.sets.indexOf(t)?this.visible=!1:this.visible=!0,this.position=this.positions[t],"blink"===this.name&&(this.visible=!1)}draw(t,e){!0===this.visible&&(this.alpha&&(t.globalAlpha=(255-this.alpha)/255),t.drawImage(this.image,this.position.x+this.offset.x,this.position.y+this.offset.y),t.globalAlpha=1,e.drawImage(this.ghostImage,this.position.x+this.offset.x,this.position.y+this.offset.y))}}class e{constructor(t,e,s){return this.currentSet=0,this.color=e,this.positions=s.map((t=>"no object"===t?{x:0,y:0}:t)),this.cels=[],this}update(t){this.currentSet=t;for(let e=0;e<this.cels.length;e++)this.cels[e].update(t)}get position(){return this.positions[this.currentSet]}get fixed(){return this.cels.some((t=>t.fix>0))}setPosition(t,e){this.positions[this.currentSet].x=t,this.positions[this.currentSet].y=e}}class s{constructor(t,e){this.size={x:t.window_size[0],y:t.window_size[1]};document.getElementById("borderarea").style.background=t.border;const s=document.getElementById("playarea");return s.style.width=this.size.x+"px",s.style.height=this.size.y+"px",s.style.background=t.background,this.initCanvases(this.size),this.currentSet=0,this.objs=[],this.cels=[],this.colorids=[],this.init(t.cels,t.positions,e),function(t){const e=document.getElementsByClassName("set");for(let s=0;s<e.length;s++)e[s].addEventListener("click",(function(){const e=parseInt(this.innerHTML);t.update(e),t.draw(),i(t.currentSet)}));const s=document.getElementsByClassName("tip")[0];document.getElementById("open-tip").addEventListener("click",(t=>{s.style.display="block"}));document.getElementById("close-tip").addEventListener("click",(t=>{s.style.display="none"}))}(this),this.update(),this.draw(),this}init(s,i,n){s.reverse(),s.forEach((s=>{const o=this.objs[s.mark];if(o){const e=new t(o,s,this,n);o.cels.push(e),this.cels.push(e)}else{const o=function(t){let e=0,s=0,i=0;t<255?e=t:t>255&&t<510?(e=0,i=t):t>510&&t<765&&(i=0,s=t);return{red:e,green:i,blue:s,alpha:255}}(s.mark),h=o.red+o.green+o.blue+255;this.colorids[h]=s.mark;const r=new e(s.mark,o,i.map((t=>t.positions[s.mark]))),a=new t(r,s,this,n);r.cels.push(a),this.objs[s.mark]=r,this.cels.push(a)}}))}initCanvases(t){const e=document.getElementById("screen");n(e,t);const s=document.getElementById("ghost");n(s,t),this.canvas=e,this.ctxt=e.getContext("2d"),this.ghost=s.getContext("2d"),e.addEventListener("touchstart",(function(t){t.preventDefault()})),e.addEventListener("touchmove",(function(t){t.preventDefault()})),e.addEventListener("touchend",(function(t){t.preventDefault()})),e.addEventListener("touchcancel",(function(t){t.preventDefault()}))}getSelectedObject(t){const e=this.ghost.getImageData(t.x,t.y,1,1).data,s=e[0]+e[1]+e[2]+255;if(0===e[3])console.log("not draggable");else{const t=this.colorids[s],e=this.objs[t];if(e&&!e.fixed)return e}}getObjectPosition(t){return t.positions[this.currentSet]}moveObject(t,e,s){t.setPosition(e,s),this.update(),this.draw()}update(t){void 0!==t&&(this.currentSet=t);for(let t=0;t<this.objs.length;t++)this.objs[t]&&this.objs[t].update(this.currentSet)}draw(){this.ctxt.clearRect(0,0,this.size.x,this.size.y),this.ghost.clearRect(0,0,this.size.x,this.size.y);for(let t=0;t<this.cels.length;t++)this.cels[t]&&this.cels[t].draw(this.ctxt,this.ghost)}}function i(t){const e=document.getElementsByClassName("set");for(let s=0;s<e.length;s++)t===parseInt(e[s].innerHTML)?e[s].style.color="black":e[s].style.color="grey"}function n(t,e){t.style.width=e.x+"px",t.style.height=e.y+"px",t.width=e.x,t.height=e.y}class o{constructor(t){this.dragHandler=!1,this.doll=t,this.screen=document.getElementById("screen");const e=document.getElementById("ghost");this.ctxt=e.getContext("2d")}initialize(){document.addEventListener("pointerdown",(t=>this.onMouseDown(t))),document.addEventListener("pointerup",(()=>{this.dragHandler&&(document.removeEventListener("pointermove",this.dragHandler),this.dragHandler=!1)}))}onMouseDown(t){const e=this.getMousePos(t),s=this.doll.getSelectedObject(e);if(s){const i=this.getDragStart(s,e);this.dragHandler=t=>this.onMouseMove(t,s,i),document.addEventListener("pointermove",this.dragHandler),t.preventDefault()}}getDragStart(t,e){const s=this.doll.getObjectPosition(t);return{x:s.x-e.x,y:s.y-e.y}}onMouseMove(t,e,s){const i=this.getMousePos(t),n=s.x+i.x,o=s.y+i.y;this.doll.moveObject(e,n,o),t.preventDefault()}getMousePos(t){const e=this.screen.getBoundingClientRect();return{x:t.clientX-e.left,y:t.clientY-e.top}}}window.addEventListener("load",(function(){let t=0;const e=kissJson.cels.length,i=new s(kissJson,(()=>{t+=1}));new o(i).initialize(),window.setTimeout((function s(){t<e?(console.log(`loading ${t} of ${e}`),window.setTimeout(s,500)):i.draw()}),500)}))})();
//# sourceMappingURL=doll.js.map
