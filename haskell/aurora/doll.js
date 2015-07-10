/* SMOOCH */

;(function() {

  var KissSet = function(kissData) {
  
    //var screen = document.getElementById("screen").getContext("2d");
    var screen;
    this.size = { }
    //this.size = { x: screen.canvas.width, y: screen.canvas.height };
     
    this.mouse = Mouser();

    var cells = [];
    var objs = kissData.objs;
    for (var i = 0; i < objs.length; i++) {
      currCells = objs[i].cells;
      for (var j = 0; j < currCells.length; j++) {
        currCells[j].object = objs[i].id;
        currCells[j].position = objs[i].positions[1];
      }
      cells = cells.concat(currCells);
    }

    this.images = []; 
    for (var i = 0; i < cells.length; i++) {
      this.images.push(document.getElementById(cells[i].name));
      /* find obj for image */
      /* find coords for that obj in set 1 */
      /* set image coords */
    }

    for (var i = 0; i < this.images.length; i++) {
      this.images[i].style.top  = cells[i].position.y + "px";
      this.images[i].style.left = cells[i].position.x + "px";
    }

  };

  var Mouser = function() {

    var isdrag = false;
    var x, y;
    var dojb;

    var mousemove = function(e) {
      if (isdrag) {
        dobj.style.left = parseInt(tx + e.clientX - x,10) + "px";
        dobj.style.top = parseInt(ty + e.clientY - y,10) + "px";
        return false;
      }
    };

    var selectmouse = function(e) {
      var fobj = e.target;

      if (fobj.draggable) {
        isdrag = true;
        dobj = fobj;
        tx = parseInt(dobj.style.left + 0);
        ty = parseInt(dobj.style.top + 0);
        x = e.clientX;
        y = e.clientY;
        document.onmousemove=mousemove;
        return false;
      }
    };

    document.onmousedown = selectmouse;
    document.onmouseup = function () {
      isdrag = false;
    };

  };

  window.addEventListener('load', function() {
    var kissData = kissJson;
    new KissSet(kissData);
  });

})();
