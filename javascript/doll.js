/* SMOOCH */

const colorids = []
let loaded = 0

const Smooch = function (kissData) {
  this.doll = new KissDoll(kissData)
  this.doll.update()
  this.doll.draw()

  // This controls dragging and dropping.
  this.mouse = Mouser(this)

  return this
}

const KissDoll = function (kissData) {
  // Size of the play area.
  this.size = { x: kissData.window_size[0], y: kissData.window_size[1] }

  // Set up border area (around the playarea)
  const borderarea = document.getElementById('borderarea')
  borderarea.style.background = kissData.border

  // Set up play area
  const playarea = document.getElementById('playarea')
  playarea.style.width = this.size.x + 'px'
  playarea.style.height = this.size.y + 'px'
  playarea.style.background = kissData.background

  // Set up canvases
  this.initCanvases()

  // Initalize click handler for set number listing
  this.initSetClicks()
  this.currentSet = 0

  // Initialize objs and cels
  this.objs = []
  this.cels = []
  this.init(kissData.objs, kissData.cels)

  return this
}

KissDoll.prototype = {
  init: function (objs, cels) {
    /* Cels have to be kept in a separate list from the objects.
           This is because objects are the things that have click handlers,
           get dragged, change position, etc. But cels are the things that
           are drawn, and they have to be drawn in a certain order to preserve
           the layering effect. */

    // Helper function for matching objects and cels
    const matches = function (objCel, cel) {
      const unmatched = !objCel.matched
      const nameMatches = objCel.name === cel.name
      const palMatches = objCel.palette === cel.palette
      return unmatched && nameMatches && palMatches
    }

    /* Go through each KiSS object, add information from the object to the
           cels within the object, then add those cels to the list. */

    let red = 0
    let blue = 0
    let green = 0

    for (let i = 0; i < objs.length; i++) {
      // create a unique color for each obj based on the obj id
      // and register it in the global colorids array
      // supports up to 255*3 objects
      if (i < 255) {
        red = i
      } else if (i > 255 && i < 255 * 2) {
        red = 0
        green = i
      } else if (i > 255 * 2 && i < 255 * 3) {
        green = 0
        blue = i
      }

      const colorid = red + green + blue + 255
      colorids[colorid] = i
      objs[i].color = { red: red, green: green, blue: blue, alpha: 255 }

      // now lets go through the cels
      const objCels = objs[i].cels
      // for each cel in the obj, find that cel in the celData list
      for (let j = 0; j < objCels.length; j++) {
        for (let k = 0; k < cels.length; k++) {
          // The only way we can match cels is by name.
          // If multiple objects share the same cel, the cel list has
          // multiple copies of the cel, and we skip cels that are already
          // matched. This is hacky as hell.
          // TODO: This is going to cause trouble if we have multiple cels with
          // the same name, but they have different palettes applied.
          // Could make sure that the palette of the object and the cel match
          if (matches(objCels[j], cels[k], this.cels[k])) {
            if (this.cels[k] && this.cels[k].obj) {
              console.log('already matched')
            } else {
              objCels[j] = new KiSSCel(objs[i], cels[k], this)
              this.cels[k] = objCels[j]
              objCels[j].matched = true
            }
          }
        }
      }

      this.objs[i] = new KiSSObj(objs[i])
    }

    // cels have to be drawn in reverse order (drawing lowest items first)
    this.cels.reverse()
  },

  initCanvases: function () {
    const drawcanvas = document.getElementById('screen')
    const drawctxt = drawcanvas.getContext('2d')
    drawcanvas.style.width = this.size.x + 'px'
    drawcanvas.style.height = this.size.y + 'px'
    drawcanvas.width = this.size.x
    drawcanvas.height = this.size.y

    const ghostcanvas = document.getElementById('ghost')
    const ghostctxt = ghostcanvas.getContext('2d')
    ghostcanvas.style.width = this.size.x + 'px'
    ghostcanvas.style.height = this.size.y + 'px'
    ghostcanvas.width = this.size.x
    ghostcanvas.height = this.size.y
    ghostcanvas.style.background = 'blue'
    ghostcanvas.style.display = 'none'

    this.ctxt = drawctxt
    this.canvas = drawcanvas
    this.ghost = ghostctxt
  },

  initSetClicks: function () {
    // Add click events to set numbers
    this.sets = document.getElementsByTagName('a')
    for (let i = 0; i < this.sets.length; i++) {
      const that = this
      // when a number is clicked, set current set to that number, update
      this.sets[i].addEventListener('click', function () {
        that.currentSet = parseInt(this.innerHTML)
        that.update()
        that.draw(that.ctxt, that.ghost)
      })
    }
  },

  update: function () {
    // Update cels
    for (let i = 0; i < this.objs.length; i++) {
      this.objs[i].update(this)
    }

    // Update set listing
    for (let i = 0; i < this.sets.length; i++) {
      if (this.currentSet === parseInt(this.sets[i].innerHTML)) {
        this.sets[i].style.color = 'black'
      } else {
        this.sets[i].style.color = 'grey'
      }
    }
  },

  draw: function () {
    this.ctxt.clearRect(0, 0, this.size.x, this.size.y)
    this.ghost.clearRect(0, 0, this.size.x, this.size.y)
    for (let i = 0; i < this.cels.length; i++) {
      if (this.cels[i]) {
        this.cels[i].draw(this.ctxt, this.ghost)
      }
    }
  }
}

const KiSSObj = function (obj) {
  this.currentSet = 0
  this.positions = obj.positions
  this.position = obj.positions[this.currentSet]
  this.cels = obj.cels

  return this
}

KiSSObj.prototype = {
  update: function (that) {
    for (let i = 0; i < this.cels.length; i++) {
      this.cels[i].currentSet = that.currentSet
      this.cels[i].position = this.positions[that.currentSet]
      this.cels[i].update(that)
    }
  }
}

const KiSSCel = function (obj, cel, set) {
  this.obj = obj
  this.name = cel.name
  this.mark = obj.id
  this.fix = cel.fix
  this.position = obj.positions[0]
  this.positions = obj.positions
  this.sets = cel.sets
  this.image = undefined
  this.ghostImage = undefined
  this.visible = false
  this.alpha = cel.alpha

  this.offset = cel.offset

  this.init(set)

  return this
}

KiSSCel.prototype = {
  init: function (set) {
    const drawctxt = set.ctxt
    const drawcanvas = set.canvas

    const image = document.getElementById(this.name)
    const width = image.width
    const height = image.height

    this.image = image

    // Draw image to ctxt and get image data
    drawctxt.drawImage(image, 0, 0, width, height)

    const ghostImageData = drawctxt.getImageData(0, 0, width, height)
    const data = ghostImageData.data

    // Fill image data with obj color
    const color = this.obj.color

    for (let k = 0; k < data.length; k = k + 4) {
      data[k] = color.red
      data[k + 1] = color.green
      data[k + 2] = color.blue
    }

    // Clear ctxt and draw altered image
    drawctxt.clearRect(0, 0, set.size.x, set.size.y)
    drawctxt.putImageData(ghostImageData, 0, 0)

    // Save altered image as cel's ghost image
    this.ghostImage = new Image()
    this.ghostImage.src = drawcanvas.toDataURL('image/png')

    // Let Smooch know when image is loaded
    this.ghostImage.onload = function () {
      loaded = loaded + 1
    }

    // Clear ctxt
    drawctxt.clearRect(0, 0, set.size.x, set.size.y)
  },

  update: function (that) {
    if (this.sets.indexOf(that.currentSet) === -1) {
      this.visible = false
    } else {
      this.visible = true
    }
    if (this.name === 'blink') {
      this.visible = false
    }
  },

  draw: function (screen, ghost) {
    if (this.visible === true) {
      if (this.alpha) {
        screen.globalAlpha = (255 - this.alpha) / 255
      }

      screen.drawImage(
        this.image,
        this.position.x + this.offset.x,
        this.position.y + this.offset.y
      )

      screen.globalAlpha = 1

      ghost.drawImage(
        this.ghostImage,
        this.position.x + this.offset.x,
        this.position.y + this.offset.y
      )
    }
  }
}

const Mouser = function (that) {
  // This is partly from eLouai
  // http://www.elouai.com/javascript-drag-and-drop.php
  let isdrag = false
  let tx, ty, x, y, dobj, curSet
  const screen = document.getElementById('screen')

  const mousemove = function (e) {
    if (isdrag) {
      const pos = getMousePos(screen, e)
      dobj.positions[curSet].x = tx + pos.x - x
      dobj.positions[curSet].y = ty + pos.y - y
      that.doll.update()
      that.doll.draw()
      return false
    } else {
      return true
    }
  }

  // from http://www.html5canvastutorials.com/advanced/html5-canvas-mouse-coordinates/
  const getMousePos = function (canvas, evt) {
    const rect = canvas.getBoundingClientRect()
    return {
      x: evt.clientX - rect.left,
      y: evt.clientY - rect.top
    }
  }

  const selectmouse = function (e) {
    const canvas = document.getElementById('ghost')
    const ctxt = canvas.getContext('2d')

    const pos = getMousePos(screen, e)

    const pixel = ctxt.getImageData(pos.x, pos.y, 1, 1)

    const data = pixel.data

    const colorid = data[0] + data[1] + data[2] + 255

    if (data[3] === 0) {
      console.log('not draggable')
      return true
    } else {
      const kobj = that.doll.objs[colorids[colorid]]
      if (kobj && kobj.cels[0].fix < 1) {
        isdrag = true
        dobj = kobj
        curSet = that.doll.currentSet
        tx = dobj.positions[curSet].x
        ty = dobj.positions[curSet].y
        x = pos.x
        y = pos.y
        document.onmousemove = mousemove
        return false
      } else {
        return true
      }
    }
  }

  document.onmousedown = selectmouse
  document.onmouseup = function () {
    isdrag = false
  }

  this.getSelected = function () {
    return this.selectedObj
  }
}

window.addEventListener('load', function () {
  /* globals kissJson */
  this.smooch = new Smooch(kissJson)
  const checkLoaded = function () {
    if (loaded < kissJson.cels.length) {
      console.log('loading...')
      window.setTimeout(checkLoaded, 500)
    } else {
      this.smooch.doll.draw()
    }
  }

  window.setTimeout(checkLoaded, 500)
})
