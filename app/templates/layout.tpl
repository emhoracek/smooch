<!DOCTYPE html>
<html>
  <head>
    <title>Smooch!</title>
    <meta name="viewport" content="width=device-width, initial-scale=1"/>
    <!-- Reset stylesheet -->
    <link rel="stylesheet" type="text/css" href="/static/reset.css">
    <!-- Page stylesheet -->
    <link rel='stylesheet' type='text/css' href='/static/main.css' />
    <!-- Doll stylesheet -->
    <link rel='stylesheet' type='text/css' href='/static/screen.css' />
    <!-- Script for dolls -->
    <script type='text/javascript'>
     window.onload = function () {
         document.getElementById('loading').style.display = 'none';
     };
    </script>

    <!-- Doll scripts -->
    <script type='text/javascript' src='/static/yura/setdata.js'></script>
    <script type='text/javascript' src='/static/doll.js'></script>
  </head>
  <body>
    <!-- Header and logo -->
    <header>
      <h1 class="logo">Smooch</h1>
      <span class="alpha">alpha</span>
      <nav>
        <ul>
          <li><a href="/">Home</a></li>
          <li><a href="/where-to-find">Where to find dolls</a></li>
          <li><a href="/about">About</a></li>
          <li><a href="https://github.com/emhoracek/smooch">GitHub</a></li>
        </ul>
      </nav>
    </header>

    <div class="wrapper">
      <apply-content />
    </div>
  </body>
</html>