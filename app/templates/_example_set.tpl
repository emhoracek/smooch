<section class="demo">
  <div id="wrapper">
    <div id="loading">
      <p>Loading...</p>
      <progress></progress>
    </div>

    <header>
      <h2>Featured doll</h2>
    </header>

    <div id="menu">
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
        <button id="documents-button" class="documents-button">Documents</button>
      </div>

      <p class="tip">Click through the doll's sets of clothing by choosing a number. <button id="close-tip">Close</button></p>
      <div id="documents" class="documents">
        <ul id="document-list">
          <li>kayla.cnf -
                  <a class="view-file" data-filename="kayla.cnf" href="/static/kayla/kayla.cnf">view</a> /
                  <a class="dl-file"  download="kayla.cnf" href="/static/kayla/kayla.cnf">download</a>
          </li>
        </ul>
      </div>
    </div>

    <div id="borderarea">
      <div id="playarea">
        <canvas id="screen"></canvas>
        <canvas id="ghost"></canvas>
      </div>
    </div>

    <div id="credits">
      <strong>Kayla</strong> by <a href="https://linktr.ee/Chococat0w0">Shania Waul</a> (2022) (<a href="/static/kayla/kayla.lzh" download="kayla.lzh">download</a>)</a>
    </div>

    <div id="notepad-modal" class="notepad-modal">
      <header>
        <h2 id="notepad-title"></h2>
        <button id="notepad-close">&#x2715</button>
      </header>

      <pre></pre>
    </div>

    <div id="set-data" data-static-directory="/static/kayla"></div>
  </div>
</section>
