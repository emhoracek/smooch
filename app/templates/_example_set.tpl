<section class="demo">
  <div id="wrapper">
    <div id="loading">
      <p>Loading...</p>
      <progress></progress>
    </div>

    <div id="sets">
      <ul>
        <li><button class="set">0</button></li>
        <li><button class="set">1</button></li>
        <li><button class="set">2</button></li>
        <li><button class="set">3</button></li>
        <li><button class="set">4</button></li>
        <li><button class="set">5</button></li>
        <li><button class="set">6</button></li>
        <li><button class="set">7</button></li>
        <li><button class="set">8</button></li>
        <li><button class="set">9</button></li>
        <li><button id="open-tip">?</button></li>
      </ul>
      <p class="tip">Click through the set by choosing a number. <button id="close-tip">Close</button></p>
    </div>
    <div id="borderarea">
      <div id="playarea">
        <canvas id="screen"></canvas>
        <canvas id="ghost"></canvas>
      </div>
    </div>

    <div id="set-data" data-static-directory="/static/yura"></div>
  </div>
</section>
