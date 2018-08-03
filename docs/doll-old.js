/* SMOOCH */

var colorids = [];

var Smooch = function(kissData) {

    this.editMode = false;

    var editButton = document.getElementById("edit");

    var that = this;

    // The color of area around the play set.
    this.borderarea = document.getElementById("borderarea");
    borderarea.style.background = kissData.border;

    this.set = new KissSet(kissData);

    // This controls dragging and dropping.
    this.mouse = Mouser(this);

    return this;

};

var KissSet = function(kissData) {

    // Size of the play area.
    this.size = { x: kissData.window_size[0],
                  y: kissData.window_size[1] };

    var playarea = document.getElementById("playarea");
    playarea.style.width = this.size.x + "px";
    playarea.style.height = this.size.y + "px";
    playarea.style.background = kissData.background;

    var drawcanvas = document.getElementById("screen");
    var drawctxt = drawcanvas.getContext("2d");
    drawcanvas.style.width = this.size.x + "px";
    drawcanvas.style.height = this.size.y + "px";
    drawcanvas.width = this.size.x;
    drawcanvas.height = this.size.y;

    var ghostcanvas = document.getElementById("ghost");
    var ghostctxt = ghostcanvas.getContext("2d");
    ghostcanvas.style.width  = (this.size.x) + "px";
    ghostcanvas.style.height = (this.size.y) + "px";
    ghostcanvas.width = this.size.x;
    ghostcanvas.height = this.size.y;
    ghostcanvas.style.background = "blue";
    ghostcanvas.style.display = "none";

    this.ctxt = drawctxt;
    this.canvas = drawcanvas;
    this.ghost = ghostctxt;

    // Start with set 0.
    this.currentSet = 0;

    /* Build a list of the cels (images) in the doll. */

    /* Go through each KiSS object, add information from the object to the
       cels within the object, then add those cels to the list. */

    this.cels = kissData.cels;
    var objs = kissData.objs;
    this.objs = [];

    for (var i = 0; i < objs.length; i++) {
        var objid = objs[i].id;

        // create a unique color for each obj based on the obj id
        // and register it in the global colorids array
        if (i < 255) {
            var colorid = i + 0 + 0 + 255;
            colorids[colorid] = i;
            objs[i].color = { red: i, green: 0, blue: 0, alpha: 255 };
        }

        // now lets go through the cels
        var obj_cels = objs[i].cels;
        // for each cel in the obj, find that cel in the celData list
        for (var j = 0; j < obj_cels.length; j++) {
            for (var k = 0; k < this.cels.length; k++) {
                if (this.cels[k].name === obj_cels[j].name) {
                    obj_cels[j] = new KiSSCel(objs[i], this.cels[k], this);
                    this.cels[k] = obj_cels[j];
                }
            }
        }

        this.objs[i] = new KiSSObj(objs[i]);
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
            that.ghost.clearRect(0,0, that.size.x, that.size.y);
            that.draw(drawctxt, ghostctxt);
        });
    }

    this.cels.reverse();

    this.currentSet = 0;
    this.update();
    this.draw(drawctxt, ghostctxt);
};

KissSet.prototype = {
    update: function () {

        // Update cels
        for (var i = 0; i < this.objs.length; i++) {
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
        screen.clearRect(0, 0, this.size.x, this.size.y);
        ghost.clearRect(0, 0, this.size.x, this.size.y);
        for (var i = 0; i < this.cels.length; i++) {
            if (this.cels[i]) {
                this.cels[i].draw(screen, ghost);
            }
        }
    }
};

var KiSSObj = function (obj) {
    this.currentSet = 0;
    this.positions = obj.positions;
    this.position = obj.positions[this.currentSet];
    this.cels = obj.cels;
}

KiSSObj.prototype = {
    update: function(that) {
        for (var i = 0; i < this.cels.length; i++) {
            this.cels[i].currentSet = that.currentSet;
            this.cels[i].position = this.positions[that.currentSet];
            this.cels[i].update(that);
        }
    }
};

var KiSSCel = function(obj, cel, set) {

    this.obj = obj;
    this.name = cel.name;
    this.mark = obj.id;
    this.fix = cel.fix;
    this.position = { x: 0, y: 0};
    this.positions = obj.positions;
    this.sets = cel.sets;
    this.image = undefined;
    this.ghostImage = undefined;
    this.visible = false;
    this.alpha = cel.alpha;

    this.offset = cel.offset;

    this.init(set);

    return this;
};

KiSSCel.prototype = {

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
                           width,
                           height);
        var ghostImageData = drawctxt.getImageData(0, 0,
                                                   width,
                                                   height);
        var data = ghostImageData.data;

        // Fill image data with obj color
        color = this.obj.color;

        for (var k = 0; k < data.length; k=k+4) {
            data[k]   = color.red;
            data[k+1] = color.green;
            data[k+2] = color.blue;
        }

        // Clear ctxt and draw altered image
        drawctxt.clearRect(0, 0, set.size.x, set.size.y);
        drawctxt.putImageData(ghostImageData, 0, 0);

        // Save altered image as cel's ghost image
        this.ghostImage = new Image();
        this.ghostImage.src = drawcanvas.toDataURL('image/png');

        // Clear ctxt
        drawctxt.clearRect(0, 0, set.size.x, set.size.y);

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
            if (this.alpha) {
                screen.globalAlpha = (255-this.alpha)/255;
            }
            screen.drawImage(this.image,
                             this.position.x + this.offset.x, this.position.y + this.offset.y);

            screen.globalAlpha = 1;
            ghost.drawImage(this.ghostImage,
                            this.position.x + this.offset.x, this.position.y + this.offset.y);
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
        dobj.positions[curSet].x = tx + e.layerX - x;
        dobj.positions[curSet].y = ty + e.layerY - y;
        that.set.update(that.set);
        that.set.draw(that.set.ctxt, that.set.ghost);
        return false;
      } else {
        return true;
      }
    };

      // from http://www.html5canvastutorials.com/advanced/html5-canvas-mouse-coordinates/
    var getMousePos = function(canvas, evt) {
        var rect = canvas.getBoundingClientRect();
        return {
            x: evt.clientX - rect.left,
            y: evt.clientY - rect.top
        };
    };

    var selectmouse = function(e) {

      var screen = document.getElementById('screen');
      var canvas = document.getElementById('ghost');
      var ctxt = canvas.getContext('2d');

      var pos = getMousePos(screen, e);

      var pixel = ctxt.getImageData(pos.x, pos.y, 1, 1);

      var data = pixel.data;
      var rgba = 'rgba(' + data[0] + ',' + data[1] +
          ',' + data[2] + ',' + data[3] + ')';

      var colorid = data[0] + data[1] + data[2] + 255;

      if (data[3] === 0) {
          console.log("not draggable");
          return true;
      }
      else {
        var kobj = that.set.objs[colorids[colorid]];
        if (kobj && kobj.cels[0].fix < 1) {
          isdrag = true;
          dobj = kobj;
          curSet = that.set.currentSet;
          tx = dobj.positions[curSet].x;
          ty = dobj.positions[curSet].y;
          x = e.layerX;
          y = e.layerY;
          document.onmousemove = mousemove;
          return false;
        } else {
          return true;
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
    this.smooch = new Smooch(kissData);
  });

var debug_draw = function (x) {
  this.smooch.set.objs[x].cels[0].draw(this.smooch.set.ctxt, this.smooch.set.ghost);
}
