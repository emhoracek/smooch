/* SMOOCH */
import { KiSSDoll } from './kissDoll'
import { DragAndDrop } from './dragAndDrop'

window.addEventListener('load', function () {
  /* globals kissJson */
  let loaded = 0
  const totalCels = kissJson.cels.length
  const doll = new KiSSDoll(kissJson, () => { loaded += 1 })
  const dragger = new DragAndDrop()
  dragger.initialize(doll)

  function checkLoaded () {
    if (loaded < totalCels) {
      console.log(`loading ${loaded} of ${totalCels}`)
      window.setTimeout(checkLoaded, 500)
    } else {
      doll.draw()
    }
  }

  window.setTimeout(checkLoaded, 500)
})
