// A KiSS Cel represents each instance of an image appearing in the
// doll. It doesn't represent the image file itself. So a single
// image can appear multiple times in multiple objects as different
// KiSSCels
class KiSSCel extends EventTarget {
  constructor (obj, cel, index) {
    super()

    this.name = cel.name
    this.mark = cel.mark
    this.id = cel.name
    this.index = index
    this.fix = cel.fix
    this.position = obj.positions[0]
    this.positions = obj.positions
    this.sets = cel.sets
    this.palette = cel.palette
    this.image = false
    this.ghostImage = undefined
    this.visible = false
    this.mapped = true
    this.alpha = cel.alpha
    this.currentSet = 0
    this.offset = cel.offset

    return this
  }

  loadImage (doll, incLoaded) {
    const drawctxt = doll.ctxt
    const drawcanvas = doll.canvas
    const pngLocation = `/sets/${doll.staticDirectory}/palette${this.palette}/${this.name}.png`
    const cel = this

    const image = new Image()
    image.addEventListener('load', e => {
      // Draw image to ctxt and get image data
      drawctxt.drawImage(image, 0, 0, image.width, image.height)

      const ghostImageData = drawctxt.getImageData(0, 0, image.width, image.height)
      const data = ghostImageData.data

      // Fill ghost image data with cel color
      const color = decimalToRgb(cel.index)
      for (let k = 0; k < data.length; k = k + 4) {
        data[k] = color.red
        data[k + 1] = color.green
        data[k + 2] = color.blue
      }

      // Clear ctxt and draw altered image
      drawctxt.clearRect(0, 0, doll.size.x, doll.size.y)
      drawctxt.putImageData(ghostImageData, 0, 0)

      // Save altered image as cel's ghost image
      cel.ghostImage = new Image()
      cel.ghostImage.src = drawcanvas.toDataURL('image/png')

      // Let Smooch know when image is loaded
      cel.ghostImage.addEventListener('load', e => {
        incLoaded()
      })

      cel.image = image
    })

    image.src = pngLocation
  }

  update (currentSet) {
    this.currentSet = currentSet
    if (this.sets.indexOf(currentSet) === -1 || !this.mapped) {
      this.visible = false
    } else {
      this.visible = true
    }
    this.position = this.positions[currentSet]
  }

  draw (screen, ghost) {
    if (!this.image) return false

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

  altmap () {
    this.mapped = !this.mapped
  }

  map () {
    this.mapped = true
  }

  move (dx, dy) {
    this.positions[this.currentSet].x += dx
    this.positions[this.currentSet].y += dy
  }

  setTransparency (alpha) {
    this.alpha = alpha
  }

  unmap () {
    this.mapped = false
  }
}

// thanks to DrBracewell#0252 on The Coding Den Discord
function decimalToRgb (n) {
  return {
    red: (n & 0xff0000) >> 16,
    green: (n & 0x00ff00) >> 8,
    blue: (n & 0x0000ff)
  }
}

export { KiSSCel }
