import { makeAction, objOrCelArg } from './fkissAction'

function addEvent (eventJson, doll) {
  const setEvent = eventMap[eventJson.event]

  if (setEvent) {
    const actions = eventJson.actions.map(a => makeAction(a, doll))
    setEvent(eventJson.args, actions, doll)
  } else {
    console.log('Unknown event', eventJson.event)
  }
}

const eventMap = {
  alarm: setAlarm,
  begin: setBegin,
  catch: setCatch,
  drop: setDrop,
  col: setCol,
  end: setEnd,
  fixcatch: setFixcatch,
  fixdrop: setFixdrop,
  initialize: setInitialize,
  never: setNever,
  press: setPress,
  release: setRelease,
  set: setSet,
  unfix: setUnfix
}

// Notes on each event are from Tigger's FKiSS reference: http://tigger.orpheusweb.co.uk/KISS/fkref4.html

// A timer reaches zero.
function setAlarm (args, actions, doll) {
  const index = args[0]
  doll.addEventListener('alarm' + index, (e) => {
    actions.forEach(f => f())

    doll.update()
    doll.draw()
  })
}

// This event is triggered after the initialize event and before the version event.
function setBegin (args, actions, doll) {
  doll.addEventListener('begin', (e) => actions.forEach(f => f()))
}

// The user clicks on the object or cel. Applies to all cels & objects except those with a maximal fix value.
function setCatch (args, actions, doll) {
  const objOrCel = objOrCelArg(args[0], doll)
  doll.addEventListener(`catch-${objOrCel.id}`, (e) => actions.forEach(f => f()))
}

// The user changes the palette to that specified
function setCol () {
  console.log("Not implemented: `col`. Smooch doesn't implement palette groups.")
}

// The user releases the mouse on the object or cel. Applies to all cels & objects except those with a maximal fix value.
function setDrop (args, actions, doll) {
  const objOrCel = objOrCelArg(args[0], doll)
  doll.addEventListener(`drop-${objOrCel.id}`, (e) => actions.forEach(f => f()))
}

// The user quits the player or closes the doll
function setEnd () {
  console.log('Not implemented: `end`. The browser environment makes this event impractical.')
}

// The user clicks on the object or cel. Applies only to fixed cels & objects
function setFixcatch (args, actions, doll) {
  const objOrCel = objOrCelArg(args[0], doll)
  doll.addEventListener(`fixcatch-${objOrCel.id}`, (e) => actions.forEach(f => f()))
}

// The user releases the mouse on the object or cel. Applies only to fixed cels & objects
function setFixdrop (args, actions, doll) {
  const objOrCel = objOrCelArg(args[0], doll)
  doll.addEventListener(`fixdrop-${objOrCel.id}`, (e) => actions.forEach(f => f()))
}

// Before the doll is displayed after loading
function setInitialize (args, actions, doll) {
  doll.addEventListener('initialize', (e) => actions.forEach(f => f()))
}

// This event is never triggered. Intended for debugging purposes during doll development.
function setNever () {
  // nothing happens :)
}

// The user clicks on the object or cel.
const press = new CustomEvent('press')
function setPress (args, actions, doll) {
  const objOrCel = objOrCelArg(args[0], doll)
  if (objOrCel) {
    objOrCel.addEventListener('press', (e) => actions.forEach(f => f()))
  }
}

// The user releases the object or cel.
function setRelease (args, actions, doll) {
  const objOrCel = objOrCelArg(args[0], doll)
  doll.addEventListener(`release-${objOrCel.id}`, (e) => actions.forEach(f => f()))
}

// The user changes the specified set
function setSet (args, actions, doll) {
  const set = args[0]
  doll.addEventListener(`set-${set}`, (e) => actions.forEach(f => f()))
}

// A previously-fixed cel or object becomes free to move.
function setUnfix () {
  console.log("Not implemented: `unfix`. Smooch doesn't (currently) decrement fix.")
}

export { addEvent, press }