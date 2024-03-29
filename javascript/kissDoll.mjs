import { addEvent } from './fkissEvent'
import { KiSSCel } from './kissCel.mjs'
import { KiSSSound } from './kissSound.mjs'
import { KiSSObject } from './kissObject.mjs'
import { Logger } from './logger'

class KiSSDoll extends EventTarget {
  constructor (kissData, staticDirectory) {
    super()

    this.staticDirectory = staticDirectory
    this.cnf = kissData
    this.logger = new Logger('debug')

    // Size of the play area.
    this.size = { x: kissData.window_size[0], y: kissData.window_size[1] }

    // Set colors
    this.borderColor = kissData.border
    this.backgroundColor = kissData.background

    // Initialize current set
    this.currentSet = 0

    // Initialize objs and cels
    this.objs = []
    this.cels = []
    this.sounds = []
    this.timers = []

    return this
  }

  initialize (incLoaded) {
    // Set up border area (around the playarea)
    const borderarea = document.getElementById('borderarea')
    borderarea.style.background = this.borderColor

    // Set up play area
    const playarea = document.getElementById('playarea')
    playarea.style.width = this.size.x + 'px'
    playarea.style.height = this.size.y + 'px'
    playarea.style.background = this.backgroundColor

    // Set up canvases
    this.initCanvases(this.size)

    this.initCels(this.cnf.cels, this.cnf.positions)
    initSetClicks(this)

    if (this.cnf.fkiss) {
      this.initFKiSS(this.cnf.fkiss)
    }

    this.dispatchEvent(new CustomEvent('initialize'))

    // load images for all the cels
    this.cels.forEach(c => c.loadImage(this, incLoaded))
  }

  begin () {
    // Update and draw
    this.update()
    this.draw()

    this.dispatchEvent(new CustomEvent('begin'))
  }

  initCels (cnfCels, cnfPositions, incLoaded) {
    /* Cels have to be kept in a separate list from the objects.
        This is because objects are the things that get dragged, change
        position, etc. But cels are the things that are drawn, and they have
        to be drawn in a certain order to get the right layering effect. */

    cnfCels.reverse()

    cnfCels.forEach((cnfCel, i) => {
      // Objects are indexed by their "mark". This is what groups cels together.
      // First check if there's already an object with that mark.
      const existingObj = this.objs[cnfCel.mark]
      if (existingObj) {
        // If object already exists, create a cel that points to that object.
        const newCel = new KiSSCel(existingObj, cnfCel, i)
        // Add the new cel to the object's list of cels.
        existingObj.cels.push(newCel)
        // Add the new cel to the doll's list of cels.
        this.cels.push(newCel)
      } else {
        // If the object doesn't already exist, we need to create it.

        // Create the new object and new cel
        const newObj = new KiSSObject(cnfCel.mark, cnfPositions.map(sp => sp.positions[cnfCel.mark] || { x: 0, y: 0 }))
        const newCel = new KiSSCel(newObj, cnfCel, i)
        // Add the new cel to the object's list of cels.
        newObj.cels.push(newCel)
        // Add the object to the doll's list of objects.
        this.objs[cnfCel.mark] = newObj
        // Add the cel to the doll's list of cels.
        this.cels.push(newCel)
      }
    })
  }

  initCanvases (size) {
    const drawcanvas = document.getElementById('screen')
    setCanvasSize(drawcanvas, size)

    const ghostcanvas = document.getElementById('ghost')
    setCanvasSize(ghostcanvas, size)

    this.canvas = drawcanvas
    this.ctxt = drawcanvas.getContext('2d')
    this.ghost = ghostcanvas.getContext('2d')

    drawcanvas.addEventListener('touchstart', function (event) { event.preventDefault() })
    drawcanvas.addEventListener('touchmove', function (event) { event.preventDefault() })
    drawcanvas.addEventListener('touchend', function (event) { event.preventDefault() })
    drawcanvas.addEventListener('touchcancel', function (event) { event.preventDefault() })
  }

  changeSet (newSet) {
    this.update(newSet)
    this.draw()
    updateSets(this.currentSet)
  }

  getSelectedObject (pos) {
    const pixel = this.ghost.getImageData(pos.x, pos.y, 1, 1)
    const data = pixel.data
    const index = rgbToDecimal(data[0], data[1], data[2])
    const alpha = data[3]

    if (alpha === 0) {
      console.log('not draggable')
      return false
    } else {
      const cel = this.cels[index]
      const obj = this.objs[cel.mark]
      return { object: obj, cel: cel }
    }
  }

  getObjectPosition (obj) {
    return obj.positions[this.currentSet]
  }

  getCel (celName) {
    return this.cels.find(c => (c.name + '.cel') === celName)
  }

  getObject (objMark) {
    return this.objs[objMark]
  }

  createSound (filename) {
    this.sounds[filename] = new KiSSSound(filename)
  }

  getOrCreateSound (filename) {
    const sound = this.sounds[filename]
    if (sound) {
      return sound
    } else {
      const newSound = new KiSSSound(this.staticDirectory, filename)
      this.sounds[filename] = newSound
      return newSound
    }
  }

  setTimer (alarmId, duration) {
    const timer = this.timers[alarmId]
    const currentTimeout = timer ? timer.timeout : false
    const currentCallback = timer ? timer.callback : () => {}
    if (currentTimeout) {
      clearTimeout(currentTimeout)
    }
    const timeout = setTimeout(() => {
      this.timers[alarmId].timeout = false
      this.timers[alarmId].callback()
    }, duration)

    this.timers[alarmId] = { timeout: timeout, callback: currentCallback }
  }

  moveObject (obj, x, y) {
    obj.setPosition(x, y)

    this.update()
    this.draw()
  }

  initFKiSS (events) {
    events.forEach(e => {
      addEvent(e, this)
    })
  }

  update (newSet) {
    // Update current set if new set is given
    if (typeof newSet !== 'undefined') { this.currentSet = newSet }

    // Update cels
    for (let i = 0; i < this.objs.length; i++) {
      // have to check if object exists first, because we are using
      // the object id ("mark") as the index for the array, so
      // many indexes are skipped.
      if (this.objs[i]) {
        this.objs[i].update(this.currentSet)
      }
    }
  }

  draw () {
    this.ctxt.clearRect(0, 0, this.size.x, this.size.y)
    this.ghost.clearRect(0, 0, this.size.x, this.size.y)
    this.cels.forEach(cel => cel.draw(this.ctxt, this.ghost))
  }
}

function initSetClicks (doll) {
  // Add click events to set numbers
  const sets = document.getElementsByClassName('set')
  for (let i = 0; i < sets.length; i++) {
    // when a number is clicked, update doll to new set
    sets[i].addEventListener('click', function () {
      const newSet = parseInt(this.innerHTML)
      doll.changeSet(newSet)
    })
  }

  const tip = document.getElementsByClassName('tip')[0]
  const openTip = document.getElementById('open-tip')
  openTip.addEventListener('click', e => {
    tip.style.display = 'block'
  })
  const closeTip = document.getElementById('close-tip')
  closeTip.addEventListener('click', e => {
    tip.style.display = 'none'
  })
}

function updateSets (currentSet) {
  // Update set listing to highlight current set
  const sets = document.getElementsByClassName('set')
  for (let i = 0; i < sets.length; i++) {
    if (currentSet === parseInt(sets[i].innerHTML)) {
      sets[i].style.color = 'black'
    } else {
      sets[i].style.color = 'grey'
    }
  }
}

function setCanvasSize (canvas, size) {
  canvas.style.width = size.x + 'px'
  canvas.style.height = size.y + 'px'
  canvas.width = size.x
  canvas.height = size.y
}

function rgbToDecimal (r, g, b) {
  return (r << 16) + (g << 8) + b
}

export { KiSSDoll }
