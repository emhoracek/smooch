<html>
  <head>
    <title>Smooch!</title>
    <!-- Reset stylesheet -->
    <link rel="stylesheet" type="text/css" href="../static/reset.css">
    <!-- Doll stylesheet -->
    <link rel='stylesheet' type='text/css' href='../static/screen.css' />
    <!-- Script for dolls -->
    <script type='text/javascript'>
     window.onload = function () {
         document.getElementById('loading').style.display = 'none';
     };
    </script>
  </head>
  <body>
    <!-- Header and logo -->
    <header>
      <h1 class="logo">Smooch</h1>
    </header>

    <if exists="${loggedInUser}">
      <then> Hello, <loggedInUser />! </then>
    </if>

    <main>
      <!-- TODO: Maybe should be a partial? -->
      <section class="demo">
        
      <!-- Demo doll should go here -->
      <div id='wrapper'>
        <div id='loading'>
          <p>Loading...</p>
          <progress></progress>
        </div>

        <div id='sets'>
          <ul>
            <li><a class='set'>0</a></li><li><a class='set'>1</a></li><li><a class='set'>2</a></li><li><a class='set'>3</a></li><li><a class='set'>4</a></li><li><a class='set'>5</a></li><li><a class='set'>6</a></li><li><a class='set'>7</a></li><li><a class='set'>8</a></li><li><a class='set'>9</a></li>
          </ul>
          <p>Click through the set by choosing a number.</p>
        </div>
        <!-- NOTE: I had to change the path, might have to think about making a partial to make it dynamic and not have to write a path -->
        <div id='borderarea'>
          <div id='playarea'>
            <canvas id='screen'></canvas>
            <canvas id='ghost'></canvas>
            <img src='../static/yura/yura.png' id='yura' /><img src='../static/yura/logo.png' id='logo' /><img src='../static/yura/guild.png' id='guild' /><img src='../static/yura/neck9b.png' id='neck9b' /><img src='../static/yura/neck6b.png' id='neck6b' /><img src='../static/yura/top20b.png' id='top20b' /><img src='../static/yura/vest1b.png' id='vest1b' /><img src='../static/yura/top15b.png' id='top15b' /><img src='../static/yura/swordb.png' id='swordb' /><img src='../static/yura/purse4b.png' id='purse4b' /><img src='../static/yura/purse3b.png' id='purse3b' /><img src='../static/yura/dress1b.png' id='dress1b' /><img src='../static/yura/top14b.png' id='top14b' /><img src='../static/yura/top13b.png' id='top13b' /><img src='../static/yura/top12b.png' id='top12b' /><img src='../static/yura/top11b.png' id='top11b' /><img src='../static/yura/skirt7b.png' id='skirt7b' /><img src='../static/yura/top9b.png' id='top9b' /><img src='../static/yura/top8b.png' id='top8b' /><img src='../static/yura/purse2b.png' id='purse2b' /><img src='../static/yura/top7b.png' id='top7b' /><img src='../static/yura/belt1b.png' id='belt1b' /><img src='../static/yura/top6b.png' id='top6b' /><img src='../static/yura/shoes5b.png' id='shoes5b' /><img src='../static/yura/purse1b.png' id='purse1b' /><img src='../static/yura/top5b.png' id='top5b' /><img src='../static/yura/top3b.png' id='top3b' /><img src='../static/yura/top1b.png' id='top1b' /><img src='../static/yura/top2b.png' id='top2b' /><img src='../static/yura/jean1b.png' id='jean1b' /><img src='../static/yura/myo.png' id='myo' /><img src='../static/yura/myo2.png' id='myo2' /><img src='../static/yura/und1.png' id='und1' /><img src='../static/yura/socks1.png' id='socks1' /><img src='../static/yura/socks4.png' id='socks4' /><img src='../static/yura/socks3.png' id='socks3' /><img src='../static/yura/socks2.png' id='socks2' /><img src='../static/yura/socks6.png' id='socks6' /><img src='../static/yura/socks5.png' id='socks5' /><img src='../static/yura/shoes4.png' id='shoes4' /><img src='../static/yura/shoes3.png' id='shoes3' /><img src='../static/yura/shoes2.png' id='shoes2' /><img src='../static/yura/shoes14.png' id='shoes14' /><img src='../static/yura/shoes13.png' id='shoes13' /><img src='../static/yura/shoes12.png' id='shoes12' /><img src='../static/yura/shoes11.png' id='shoes11' /><img src='../static/yura/shoes10.png' id='shoes10' /><img src='../static/yura/shoes8.png' id='shoes8' /><img src='../static/yura/shoes6.png' id='shoes6' /><img src='../static/yura/shoes5f.png' id='shoes5f' /><img src='../static/yura/shoes9.png' id='shoes9' /><img src='../static/yura/shoes15.png' id='shoes15' /><img src='../static/yura/shoes7.png' id='shoes7' /><img src='../static/yura/jean1f.png' id='jean1f' /><img src='../static/yura/shoes1.png' id='shoes1' /><img src='../static/yura/jean2.png' id='jean2' /><img src='../static/yura/dress2.png' id='dress2' /><img src='../static/yura/dress1f.png' id='dress1f' /><img src='../static/yura/skirt11.png' id='skirt11' /><img src='../static/yura/skirt10.png' id='skirt10' /><img src='../static/yura/skirt9.png' id='skirt9' /><img src='../static/yura/skirt8.png' id='skirt8' /><img src='../static/yura/skirt7f.png' id='skirt7f' /><img src='../static/yura/skirt1.png' id='skirt1' /><img src='../static/yura/skirt14.png' id='skirt14' /><img src='../static/yura/skirt18.png' id='skirt18' /><img src='../static/yura/skirt17.png' id='skirt17' /><img src='../static/yura/skirt16.png' id='skirt16' /><img src='../static/yura/skirt15.png' id='skirt15' /><img src='../static/yura/skirt12.png' id='skirt12' /><img src='../static/yura/skirt13.png' id='skirt13' /><img src='../static/yura/skirt5.png' id='skirt5' /><img src='../static/yura/skirt4.png' id='skirt4' /><img src='../static/yura/skirt2.png' id='skirt2' /><img src='../static/yura/skirt3.png' id='skirt3' /><img src='../static/yura/skirt6.png' id='skirt6' /><img src='../static/yura/arm1.png' id='arm1' /><img src='../static/yura/top2f.png' id='top2f' /><img src='../static/yura/top1f.png' id='top1f' /><img src='../static/yura/top8f.png' id='top8f' /><img src='../static/yura/top13f.png' id='top13f' /><img src='../static/yura/top15f.png' id='top15f' /><img src='../static/yura/top12f.png' id='top12f' /><img src='../static/yura/top10.png' id='top10' /><img src='../static/yura/top3f.png' id='top3f' /><img src='../static/yura/top14f.png' id='top14f' /><img src='../static/yura/top5f.png' id='top5f' /><img src='../static/yura/top19.png' id='top19' /><img src='../static/yura/top18.png' id='top18' /><img src='../static/yura/top17.png' id='top17' /><img src='../static/yura/top16.png' id='top16' /><img src='../static/yura/top7f.png' id='top7f' /><img src='../static/yura/vest1f.png' id='vest1f' /><img src='../static/yura/top20f.png' id='top20f' /><img src='../static/yura/top11f.png' id='top11f' /><img src='../static/yura/top4.png' id='top4' /><img src='../static/yura/top9f.png' id='top9f' /><img src='../static/yura/top6f.png' id='top6f' /><img src='../static/yura/neck1.png' id='neck1' /><img src='../static/yura/belt1f.png' id='belt1f' /><img src='../static/yura/belt2.png' id='belt2' /><img src='../static/yura/purse2f.png' id='purse2f' /><img src='../static/yura/purse4f.png' id='purse4f' /><img src='../static/yura/purse3f.png' id='purse3f' /><img src='../static/yura/purse1f.png' id='purse1f' /><img src='../static/yura/swordf.png' id='swordf' /><img src='../static/yura/neck2.png' id='neck2' /><img src='../static/yura/neck3.png' id='neck3' /><img src='../static/yura/neck7.png' id='neck7' /><img src='../static/yura/neck9f.png' id='neck9f' /><img src='../static/yura/neck6f.png' id='neck6f' /><img src='../static/yura/neck5.png' id='neck5' /><img src='../static/yura/neck4.png' id='neck4' /><img src='../static/yura/scarf2.png' id='scarf2' /><img src='../static/yura/scarf4.png' id='scarf4' /><img src='../static/yura/scarf3.png' id='scarf3' /><img src='../static/yura/scarf1.png' id='scarf1' /><img src='../static/yura/hair1.png' id='hair1' />
          </div>
        </div>

        <div id='footer'>
          <p>All sets are © their original artists. Learn more about Smooch <a href='https://github.com/emhoracek/smooch'>from the Github repo</a>.</p>
        </div>
      </div>
      </section>

      <!-- End demo doll, start login -->

      <!-- Login/Signup Section-->
      <section class="authenticate">
        <!-- Signup form -->
        <div class="signup">
          <h2> Create an account</h2>

          <p>Note to contributors: you don't need to create an account unless you're
            working on users -- if you're testing dolls then you can skip to the upload form.</p>

          <form action="/users/create" method="post">
            <div>
              <label for="username">
                Username: <input name="username" />
              </label>
              <p><usernameErrors /></p>
            </div>
            <div>
              <label for="email">
                Email: <input name="email" />
              </label>
              <p><emailErrors /></p>
            </div>
            <div>
              <label for="password">
                Password: <input type="password" name="password" />
              </label>
              <p><passwordErrors /></p>
            </div>
            <div>
              <label for="password-confirmation">Password (again):
                <input type="password" name="password-confirmation" />
              </label>
            </div>

            <input type="submit" />
          </form>
        </div>  

        <!-- Login form -->
        <div class="login">
          <h2>Login</h2>

          <form action="/login" method="post">
            <div>
              <label for="username">
                Username: <input name="username" />
              </label>
            </div>
            <div>
              <label for="password">
                Password: <input type="password" name="password" />
              </label>
            </div>

            <input type="submit" />
          </form>
        </section>
      </div>
    </main>

    <!-- Doll scripts -->
    <script type='text/javascript' src='../static/yura/setdata.js'></script>

    <script type='text/javascript' src='../static/doll.js'></script>
  </body>
</html>
