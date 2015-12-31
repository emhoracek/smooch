/* SMOOCH */

;(function() {

  var colorids = [];

  var Smooch = function(kissdata) {

    this.editMode = false;

    var editButton = document.getElementById("edit");

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

    // The color of area around the play set.
    this.borderarea = document.getElementById("borderarea");
    borderarea.style.background = "pink";
    
    this.set = new KissSet(kissdata);

    // This controls dragging and dropping.
    this.mouse = Mouser(this);

    this.update();

  };

  Smooch.prototype = {
    update: function() {
      if (this.editMode == true) {
        this.toolbar.style.display = "block";
        this.borderarea.style.width = "80%";
      }
      else {
        this.toolbar.style.display = "none";
        this.borderarea.style.width = "100%";
      }
    }
  };

  var Toolbox = function () {

    var infobox = document.getElementById("info");
    
    infobox.innerHTML = dobj.name;

  };

  var KissSet = function(kissData) {
    
    // Size of the play area.
    this.size = { x: kissData.window_size[0],
                  y: kissData.window_size[1] }
    

    var playarea = document.getElementById("playarea");
    playarea.style.width = this.size.x + "px";
    playarea.style.height = this.size.y + "px";

    var drawcanvas = document.getElementById("screen");
    var drawctxt = drawcanvas.getContext("2d");
    drawcanvas.style.width = this.size.x + "px";
    drawcanvas.style.height = this.size.y + "px";
    drawcanvas.width = this.size.x;
    drawcanvas.height = this.size.y;

    
    var ghostcanvas = document.getElementById("ghost");
    var ghostctxt = ghostcanvas.getContext("2d");
    ghostcanvas.style.width  = (this.size.x / 2) + "px";
    ghostcanvas.style.height = (this.size.y / 2) + "px";
    ghostcanvas.width = this.size.x / 2;
    ghostcanvas.height = this.size.y / 2;
    ghostcanvas.style.background = "blue";
    ghostcanvas.style.display = "none";

    this.ctxt = drawctxt;
    this.canvas = drawcanvas;
    this.ghost = ghostctxt;
    
    // Start with set 0. 
    this.currentSet = 0;
    
    /* Build a list of the cells (images) in the doll.
       Go through each KiSS object, add information from the object to the 
       cells within the object, then add those cells to the list. */
    this.cells = [];
    var objs = kissData.objs;
    this.objs = []
    
    for (var i = 0; i < objs.length; i++) {
      var objid = objs[i].id
      if (i < 255) {
        var colorid = i + 0 + 0 + 255;
        colorids[colorid] = objid;
        objs[i].color = { red: i, green: 0, blue: 0, alpha: 255 };
      }
      else {
        var colorid = (i/2) + (i/2) + 0 + 255;
        colorids[colorid] = objid;
        objs[i].color = { red: i/2, green: i/2, blue: 0, alpha: 255 };
      }
      var cells = objs[i].cells;
        for (var j = 0; j < cells.length; j++) {
          cells[j] = new KiSSCell(objs[i], cells[j], this);
      }
      
      this.cells = this.cells.concat(cells);
      this.objs[objid] = new KiSSObj(objs[i]);

    }

    // Add click events to set numbers 
    this.sets = document.getElementsByTagName("a");
    for (var i = 0; i < this.sets.length; i++) {
      var that = this;
      // when a number is clicked, set current set to that number, update
      this.sets[i].addEventListener('click', function() {
        that.currentSet = parseInt(this.innerHTML);
        that.update();
        that.ctxt.clearRect(0, 0, that.size.x, that.size.y);
        that.draw(drawctxt, ghostctxt);
      });
    }

    this.cells.reverse();
    
    this.update(); 
    this.draw(drawctxt, ghostctxt);
  };

  KissSet.prototype = {
    update: function () {
     
      // Update cells
      for (var i = 0; i < this.cells.length; i++) {
        this.objs[i].update(this);
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

    draw: function(screen, ghost) {
      screen.clearRect(0,0,this.size.x, this.size.y);
      ghost.clearRect(0,0, this.size.x/2, this.size.y/2);
      for (var i = 0; i < this.cells.length; i++) {
        this.cells[i].draw(screen, ghost);
      }
    }
  };

  var KiSSObj = function (obj) {
    this.currentSet = 0;
    this.positions = obj.positions;
    this.position = obj.positions[this.currentSet];
    this.cells = obj.cells;
  }

  KiSSObj.prototype = {
    update: function(that) {
      for (var i = 0; i < this.cells.length; i++) {
        this.cells[i].currentSet = that.currentSet;
        this.cells[i].position = this.positions[that.currentSet];
        this.cells[i].update(that);
      }
    }
  };

  var KiSSCell = function(obj, cell, set) {

    this.obj = obj
    this.name = cell.name;
    this.mark = obj.mark;
    this.fix = cell.fix;
    this.position = cell.position;
    this.positions = obj.positions;
    this.sets = cell.sets;
    this.image = undefined;
    this.ghostImage = undefined;

    this.init(set);
    
    return this;
  };

  KiSSCell.prototype = {

    init: function(set) {

      var drawctxt = set.ctxt;
      var drawcanvas = set.canvas;
      var ghostctxt = set.ghost;
      
      this.image = document.getElementById(this.name);
      var width = this.image.width;
      var height = this.image.height;

      // Draw image to ctxt to get image data
      // Image is drawn 1/4 of size for the ghost canvas
      drawctxt.drawImage(this.image,
                         0,
                         0,
                         width / 2,
                         height / 2 );
      var ghostImageData = drawctxt.getImageData(0, 0,
                                                 width/2,
                                                 height/2);
      var data = ghostImageData.data;

      // Fill image data with obj color
      color = this.obj.color
      
      for (var k = 0; k < data.length; k=k+4) {
        data[k]   = color.red;
        data[k+1] = color.green;
        data[k+2] = color.blue;
      }

      // Clear ctxt and draw altered image
      drawctxt.clearRect(0, 0, drawctxt.width, drawctxt.height);
      drawctxt.putImageData(ghostImageData, 0, 0);

      // Save altered image as cel's ghost image
      this.ghostImage = new Image();
      this.ghostImage.src = drawcanvas.toDataURL('image/png');

      // Clear ctxt
      drawctxt.clearRect(0, 0, drawctxt.width, drawctxt.height);

    }, 
    
    update: function(that) {
      if (this.sets.indexOf(that.currentSet) == -1) {
        this.visible = false;
      }
      else {
        this.visible = true;
      }
      if (this.name == "blink") {
        this.visible = false;
      }
    },
    
    draw: function (screen, ghost) {
      if (this.visible == true) {
        screen.drawImage(this.image,
                         this.position.x, this.position.y);
        ghost.drawImage(this.ghostImage,
                        this.position.x/2, this.position.y/2);
      }
    }
  };

  var Mouser = function(that) {

    // This is from eLouai, believe it or not!!
    // http://www.elouai.com/javascript-drag-and-drop.php   

    var isdrag = false;
    var x, y;
    var dojb;
    
    var mousemove = function(e) {
      if (isdrag) {
        dobj.positions[curSet].x = tx + e.clientX - x;
        dobj.positions[curSet].y = ty + e.clientY - y;
        that.set.update(that.set);
        that.set.draw(that.set.ctxt, that.set.ghost);
        return false;
      }
    };

    var selectmouse = function(e) {
      var fobj = e.target;

      var canvas = document.getElementById('ghost');
      var ctxt = canvas.getContext('2d');

      var x1 = e.layerX;
      var y1 = e.layerY;
      var pixel = ctxt.getImageData(x1/2, y1/2, 1, 1);
      
      var data = pixel.data;
      var rgba = 'rgba(' + data[0] + ',' + data[1] +
          ',' + data[2] + ',' + data[3] + ')';

      var colorid = data[0] + data[1] + data[2] + 255;

      if (data[3] === 0) {
        console.log("not draggable");
      }
      else {
        var kobj = that.set.objs[colorids[colorid]];
        if (kobj && kobj.cells[0].fix < 1) {
          isdrag = true;
          dobj = kobj;
          curSet = that.set.currentSet;
          tx = dobj.positions[curSet].x;
          ty = dobj.positions[curSet].y;
          x = e.clientX;
          y = e.clientY;
          document.onmousemove = mousemove;
          return false;
        }
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
