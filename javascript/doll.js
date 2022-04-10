import { KiSSDoll } from './kissDoll.mjs'
import { DragAndDrop } from './dragAndDrop.mjs'

window.addEventListener('load', function () {
  /* globals globalKiss */
  let doll = false
  let loaded = 0

  const staticDir = document.getElementById('set-data').dataset.staticDirectory

  fetch(`${staticDir}/setdata.json`).then(resp => {
    return resp.json()
  }).then(kissJson => {
    const totalCels = kissJson.cels.length

    function checkLoaded () {
      console.log(`loading ${loaded} of ${totalCels}`)
      if (loaded < totalCels) {
        window.setTimeout(checkLoaded, 500)
      } else {
        doll.begin()
        document.getElementById('loading').style.display = 'none'
        document.getElementById('sets').style.display = 'flex'
        document.getElementById('borderarea').style.display = 'block'
      }
    }

    window.setTimeout(checkLoaded, 500)

    doll = new KiSSDoll(kissJson, staticDir)
    const dragger = new DragAndDrop(doll)
    doll.initialize(() => { loaded += 1 })
    dragger.initialize()

    setUpDocViewer()

    // in testing and development, it's convenient to have access to
    // to the kiss set object in the development console
    if (typeof globalKiss !== 'undefined') {
      // eslint-disable-next-line no-global-assign
      globalKiss = doll
    }
  })
})

function setUpDocViewer () {
  const docsButton = document.getElementById('documents-button')
  const docsDiv = document.getElementById('documents')
  docsButton.addEventListener('click', e => {
    docsDiv.classList.toggle('show-docs')
  })

  const viewLinks = document.getElementsByClassName('view-file')
  const modal = document.getElementById('notepad-modal')
  const modaltext = document.querySelector('#notepad-modal pre')
  const modalh2 = document.getElementById('notepad-title')
  for (let i = 0; i < viewLinks.length; i++) {
    viewLinks[i].addEventListener('click', e => {
      const file = e.target.href
      const filename = e.target.dataset.filename
      e.preventDefault()
      fetch(file).then(resp => {
        return resp.text()
      }).then(text => {
        modal.style.display = 'block'
        modaltext.style.display = 'block'
        modaltext.textContent = text
        modalh2.textContent = filename
        modaltext.scrollTop = 0
      })
    })
  }
  const closeBtn = document.getElementById('notepad-close')
  closeBtn.addEventListener('click', e => {
    modal.style.display = 'none'
    e.preventDefault()
  })
}
