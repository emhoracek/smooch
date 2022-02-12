/* SMOOCH */

import { KiSSDoll } from './kissDoll'

const Smooch = function (kissData, incLoaded) {
  this.doll = new KiSSDoll(kissData, incLoaded)
  this.doll.update()
  this.doll.draw()

  this.colorids = []

  // This controls dragging and dropping.
  this.mouse = Mouser(this, this.doll.colorids)

  return this
}

const Mouser = function (that, colorids) {
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
  let loaded = 0
  this.smooch = new Smooch(kissJson, () => { loaded += 1 })
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
