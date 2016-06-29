/* SMOOCH */

;(function() {

  var Smooch = function(kissdata) {
    
    // This controls dragging and dropping.
    this.mouse = Mouser(); 

    /*
    this.editMode = false;

    var editButton = document.getElementById("editbutton");
    
    var that = this;
    editButton.addEventListener("click", function() {
      if (that.editMode == true) {
        that.editMode = false;
      }
      else {
        that.editMode = true;
      }
      that.update();
    });

    this.toolbar = document.getElementById("toolbar");
    */
    
    // The color of area around the play set.
    this.borderarea = document.getElementById("borderarea");
    borderarea.style.background = "pink";
    borderarea.style.width="90%";`
    borderarea.style.overflow = "auto";
    
    this.set = new KissSet(kissdata);

    this.update();

  };

  Smooch.prototype = {
    update: function() {

      /*
      if (this.editMode == true) {
        this.toolbar.style.display = "block";
        this.borderarea.style.width = "80%";
      }
      else {
        this.toolbar.style.display = "none";
        this.borderarea.style.width = "100%";
      }*/
    }
  };

  var KissSet = function(kissData) {
    
    // Size of the play area.
    this.size = { x: kissData.window_size[0],
                  y: kissData.window_size[1] }
    

    var playarea = document.getElementById("playarea");
    playarea.style.width = this.size.x + "px";
    playarea.style.height = this.size.y + "px";
    
    // Start with set 0. 
    this.currentSet = 0;

    /* Build a list of the cells (images) in the doll.
       Go through each KiSS object, add information from the object to the 
       cells within the object, then add those cells to the list. */
    this.cells = [];
    var objs = kissData.objs;
    for (var i = 0; i < objs.length; i++) {
      var cells = objs[i].cells;
      for (var j = 0; j < cells.length; j++) {
        cells[j].mark = objs[i].id;
        cells[j].positions = objs[i].positions;
        cells[j].image = document.getElementById(cells[j].name);
        cells[j] = new KiSSCell(cells[j]);
      }
      this.cells = this.cells.concat(cells);
    }

    this.cells.reverse();

    // Add click events to set numbers 
    this.sets = document.getElementsByTagName("a");
    for (var i = 0; i < this.sets.length; i++) {
      var that = this;
      // when a number is clicked, set current set to that number, update
      this.sets[i].addEventListener('click', function() {
        that.currentSet = parseInt(this.innerHTML);
        that.update();
      });
    }

    this.update();
  };

  KissSet.prototype = {
    update: function () {
      

      // Update cells
      for (var i = 0; i < this.cells.length; i++) {
        this.cells[i].update(this);
      }

      // Update set listing
      for (var i = 0; i < this.sets.length; i++) {
        if (this.currentSet == parseInt(this.sets[i].innerHTML)) {
          this.sets[i].style.color = "black";
        }
        else {
          this.sets[i].style.color = "grey";
        }
      }
    },
  };

  var KiSSCell = function(cell) {
    
    this.name = cell.name;
    this.mark   = cell.mark;
    this.fix = cell.fix;
    this.position = cell.position;
    this.positions = cell.positions;
    this.sets = cell.sets;
    this.image = cell.image;

    if (this.fix) {
      this.image.draggable = false;
    }
    return this;
  };

  KiSSCell.prototype = {
    update: function(that) {
      this.currentSet = that.currentSet;
      this.position = this.positions[this.currentSet];
      this.image.style.top  = this.position.y + "px";
      this.image.style.left = this.position.x + "px";
      if (this.sets.indexOf(that.currentSet) == -1) {
        this.image.style.display = "none";
      }
      else {
        this.image.style.display = "block";
      }
      if (this.name == "blink") {
        this.image.style.display = "none";
      }
    },
  };

  var Mouser = function() {

    // This is from eLouai, believe it or not!!
    // http://www.elouai.com/javascript-drag-and-drop.php   

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

    this.getSelected = function() {
      return this.selectedObj;
    };

  };

  window.addEventListener('load', function() {
    var kissData = kissJson;
    new Smooch(kissData);
  });

})();
