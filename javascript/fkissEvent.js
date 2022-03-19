import { makeAction } from './fkissAction'

function addEvent (eventJson, doll) {
  const defaultEvent = () => { console.log('Unknown event', eventJson.event) }
  const setEvent = eventMap[eventJson.event] || defaultEvent
  setEvent(eventJson.args, eventJson.actions, doll)
}

const eventMap = {
  alarm: setAlarm,
  begin: setBegin
}

function setAlarm (args, actions, doll) {
  const index = args[0]
  const funcs = actions.map(a => makeAction(a, doll))
  doll.addEventListener('alarm' + index, (e) => {
    funcs.forEach(f => f())

    doll.update()
    doll.draw()
  })
}

function setBegin (args, actions, doll) {
  const funcs = actions.map(a => makeAction(a, doll))
  doll.addEventListener('begin', (e) => funcs.forEach(f => f()))
}

export { addEvent }
