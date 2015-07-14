/* SMOOCH */

;(function() {

  var KissSet = function(kissData) {
  
    var screen;
    this.size = { x: kissData.window_size[0],
                  y: kissData.window_size[1] }
     
    this.mouse = Mouser(); 

    var borderarea = 
      document.getElementById("borderarea");
    borderarea.style.background = "pink";

    var playarea = 
      document.getElementById("cellarea");
    playarea.style.width = this.size.x + "px";
    playarea.style.height = this.size.y + "px";


    this.cells = [];
    var objs = kissData.objs;
    for (var i = 0; i < objs.length; i++) {
      currCells = objs[i].cells;
      for (var j = 0; j < currCells.length; j++) {
        currCells[j].mark = objs[i].id;
        currCells[j].position = objs[i].positions[1];
        if (objs[i].fix > 0) {
          currCells[j].fix = true;
        }
        currCells[j].image = document.getElementById(currCells[j].name);
        currCells[j] = new KiSSCell(currCells[j]);
      }
      this.cells = this.cells.concat(currCells);
    }

    for (var i = 0; i < this.cells.length; i++) {
      var cstyle = this.cells[i].image.style;
      cstyle.top  = (cstyle.top + this.cells[i].position.y) + "px";
      cstyle.left = (cstyle.left + this.cells[i].position.x) + "px";
      if (this.cells[i].fix) {
        this.cells[i].image.draggable = false;
      }
    }

  };

  var KiSSCell = function(cell) {
    this.name = cell.name;
    this.id   = cell.id;
    this.fix = cell.fix;
    this.position = cell.position;
    this.image = cell.image;

    return this;
  };

  KiSSCell.prototype = {
    update: {
      //blah
      },
    draw: {
      // blah
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
        tx = parseInt(dobj.style.left,10);
        ty = parseInt(dobj.style.top, 10);
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
