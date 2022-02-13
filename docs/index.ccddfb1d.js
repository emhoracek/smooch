class t{constructor(t,e,s,i){return this.obj=t,this.name=e.name,this.mark=t.id,this.fix=e.fix,this.position=t.positions[0],this.positions=t.positions,this.sets=e.sets,this.image=document.getElementById(this.name),this.ghostImage=void 0,this.visible=!1,this.alpha=e.alpha,this.offset=e.offset,this.init(s),this.ghostImage.onload=function(){i()},this}init(t){const e=t.ctxt,s=t.canvas,i=this.image;e.drawImage(i,0,0,i.width,i.height);const o=e.getImageData(0,0,i.width,i.height),n=o.data,h=this.obj.color;for(let t=0;t<n.length;t+=4)n[t]=h.red,n[t+1]=h.green,n[t+2]=h.blue;e.clearRect(0,0,t.size.x,t.size.y),e.putImageData(o,0,0),this.ghostImage=new Image,this.ghostImage.src=s.toDataURL("image/png"),e.clearRect(0,0,t.size.x,t.size.y)}update(t){-1===this.sets.indexOf(t)?this.visible=!1:this.visible=!0,"blink"===this.name&&(this.visible=!1)}draw(t,e){!0===this.visible&&(this.alpha&&(t.globalAlpha=(255-this.alpha)/255),t.drawImage(this.image,this.position.x+this.offset.x,this.position.y+this.offset.y),t.globalAlpha=1,e.drawImage(this.ghostImage,this.position.x+this.offset.x,this.position.y+this.offset.y))}}class e{constructor(t){return this.currentSet=0,this.positions=t.positions,this.position=t.positions[this.currentSet],this.cels=t.cels,this}update(t){for(let e=0;e<this.cels.length;e++)this.cels[e].currentSet=t,this.cels[e].position=this.positions[t],this.cels[e].update(t)}setPosition(t,e,s){this.positions[t].x=e,this.positions[t].y=s}}class s{constructor(t,e){this.size={x:t.window_size[0],y:t.window_size[1]};document.getElementById("borderarea").style.background=t.border;const s=document.getElementById("playarea");return s.style.width=this.size.x+"px",s.style.height=this.size.y+"px",s.style.background=t.background,this.initCanvases(this.size),this.currentSet=0,this.objs=[],this.cels=[],this.colorids=[],this.init(t.objs,t.cels,e),function(t){const e=document.getElementsByTagName("a");for(let s=0;s<e.length;s++)e[s].addEventListener("click",(function(){const e=parseInt(this.innerHTML);t.update(e),t.draw(),i(t.currentSet)}))}(this),this.update(),this.draw(),this}init(s,i,o){const n=function(t,e){const s=!t.matched,i=t.name===e.name,o=t.palette===e.palette;return s&&i&&o};let h=0,a=0,l=0;for(let c=0;c<s.length;c++){c<255?h=c:c>255&&c<510?(h=0,l=c):c>510&&c<765&&(l=0,a=c);const r=h+l+a+255;this.colorids[r]=c,s[c].color={red:h,green:l,blue:a,alpha:255};const d=s[c].cels;for(let e=0;e<d.length;e++)for(let h=0;h<i.length;h++)n(d[e],i[h],this.cels[h])&&(this.cels[h]&&this.cels[h].obj?console.log("already matched"):(d[e]=new t(s[c],i[h],this,o),this.cels[h]=d[e],d[e].matched=!0));this.objs[c]=new e(s[c])}this.cels.reverse()}initCanvases(t){const e=document.getElementById("screen"),s=e.getContext("2d");e.style.width=t.x+"px",e.style.height=t.y+"px",e.width=t.x,e.height=t.y;const i=document.getElementById("ghost"),o=i.getContext("2d");i.style.width=t.x+"px",i.style.height=t.y+"px",i.width=t.x,i.height=t.y,i.style.background="blue",i.style.display="none",this.ctxt=s,this.canvas=e,this.ghost=o}getSelectedObject(t){const e=this.ghost.getImageData(t.x,t.y,1,1).data,s=e[0]+e[1]+e[2]+255;if(0===e[3])console.log("not draggable");else{const t=this.colorids[s],e=this.objs[t];if(e&&e.cels[0].fix<1)return e}}getObjectPosition(t){return{x:t.positions[this.currentSet].x,y:t.positions[this.currentSet].y}}moveObject(t,e,s){t.setPosition(this.currentSet,e,s),this.update(),this.draw()}update(t){t&&(this.currentSet=t);for(let t=0;t<this.objs.length;t++)this.objs[t].update(this.currentSet)}draw(){this.ctxt.clearRect(0,0,this.size.x,this.size.y),this.ghost.clearRect(0,0,this.size.x,this.size.y);for(let t=0;t<this.cels.length;t++)this.cels[t]&&this.cels[t].draw(this.ctxt,this.ghost)}}function i(t){const e=document.getElementsByTagName("a");for(let s=0;s<e.length;s++)t===parseInt(e[s].innerHTML)?e[s].style.color="black":e[s].style.color="grey"}class o{constructor(t){this.dragHandler=!1,this.doll=t,this.screen=document.getElementById("screen");const e=document.getElementById("ghost");this.ctxt=e.getContext("2d")}initialize(){document.addEventListener("mousedown",(t=>this.onMouseDown(t))),document.addEventListener("mouseup",(()=>{this.dragHandler&&(document.removeEventListener("mousemove",this.dragHandler),this.dragHandler=!1)}))}onMouseDown(t){const e=this.getMousePos(t),s=this.doll.getSelectedObject(e);if(s){const i=this.getDragStart(s,e);this.dragHandler=t=>this.onMouseMove(t,s,i),document.addEventListener("mousemove",this.dragHandler),t.preventDefault()}}getDragStart(t,e){const s=this.doll.getObjectPosition(t);return{x:s.x-e.x,y:s.y-e.y}}onMouseMove(t,e,s){const i=this.getMousePos(t),o=s.x+i.x,n=s.y+i.y;this.doll.moveObject(e,o,n),t.preventDefault()}getMousePos(t){const e=this.screen.getBoundingClientRect();return{x:t.clientX-e.left,y:t.clientY-e.top}}}window.addEventListener("load",(function(){let t=0;const e=kissJson.cels.length,i=new s(kissJson,(()=>{t+=1}));new o(i).initialize(),window.setTimeout((function s(){t<e?(console.log(`loading ${t} of ${e}`),window.setTimeout(s,500)):i.draw()}),500)}));
//# sourceMappingURL=index.ccddfb1d.js.map
