<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8" />
    <title>Smooch</title>
    <link rel="stylesheet" type="text/css" href="/static/screen.css">
  </head>
  <body>
    <div id="wrapper">
      <div id="loading">
        <p>Loading...</p>
        <progress></progress>
      </div>

      <div id="menu">
        <div id="sets">
          <ul>
            <set-listing>
              <li><button class="set"><set-number /></a></li>
            </set-listing>
            <li><button id="open-tip">?</a></li>
          </ul>
          <button id="documents-button" class="documents-button">Documents</button>
        </div>

        <p class="tip">Click through the doll's sets of clothing by choosing a number. <button id="close-tip">Close</button></p>
        <div id="documents" class="documents">
          <ul id="document-list">
            <files>
              <li><filename /> -
                    <a class="view-file" data-filename="${filename}" href="/${base}/${filename}">view</a> /
                    <a class="dl-file"  download="${filename}" href="/${base}/${filename}">download</a>
              </li>
            </files>
          </ul>
        </div>
      </div>

      <div id="borderarea">
        <div id="playarea">
            <canvas id="screen"></canvas>
            <canvas id="ghost"></canvas>
        </div>
      </div>

      <div id="set-data" data-static-directory="/${base}"></div>

      <div id="footer">
        <p>All sets are © their original artists. Learn more about Smooch <a href="https://github.com/emhoracek/smooch">from the Github repo</a>.</p>
      </div>
    </div>

    <div id="notepad-modal" class="notepad-modal">
      <header>
        <h2 id="notepad-title"></h2>
        <button id="notepad-close">&#x2715</button>
      </header>

      <pre></pre>
    </div>

    <script type="module" src="/static/doll.js"></script>
  </body>
</html>
