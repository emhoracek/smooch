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
    </header>

    <if exists="${loggedInUser}">
      <then> Hello, <loggedInUser />! </then>
    </if>

    <div class="wrapper">
      <main>
        <apply template="_example_set"></apply>

        <section class="upload">
          <div>
            <h2>View a doll</h2>

            <h3>
              Be kind to <a href="http://otakuworld.com/kiss/">OtakuWorld</a>!
            </h3>

            <p>Bandwidth costs money and we want OtakuWorld to stay around
              for a long time.
            </p>

            <p>Instead of downloading dolls from OW directly, right
              click on the download link and select
              "Copy link". Paste the link below and Smooch will download it
              for you.
            </p>

            <form action="/dolls/upload" method="post">
              <div class="wide-field">
                <label for="link">Link:</label>
                <input name="link"
                      class="link-input"
                      pattern="https?:\/\/otakuworld.com\/data\/kiss\/data\/.*\.lzh"
                      placeholder="http://otakuworld.com/data/kiss/data/y/yura.lzh" />
                <input type="submit" value="Upload!" class="field submit">
                <p><linkErrors /></p>
              </div>
            </form>

            <p>Already downloaded a doll? Upload the file below.</p>

            <form action="/dolls/upload" method="post" enctype="multipart/form-data">
              <input type="file" accept=".lzh" name="kissfile">
              <input type="submit" value="Upload!" class="field submit" />
            </form>
          </div>
        </section>
      </main>

        <!-- Login/Signup Section-->
      <section class="authenticate">
        <!-- Signup form -->
        <div class="signup">
          <h2> Create an account</h2>
          <p>Save your favorite dolls so you can play again and again.</p>
          <form action="/users/create" method="post">
            <!-- Username -->
            <div class="field">
              <label for="username">Username:</label>
              <input name="username" />
              <p><usernameErrors /></p>
            </div>
            <!-- Email -->
            <div class="field">
              <label for="email">Email:</label>
              <input name="email" />
              <p><emailErrors /></p>
            </div>
            <!-- Password -->
            <div class="field">
              <label for="password">Password:</label>
              <input type="password" name="password" />
              <p><passwordErrors /></p>
            </div>
            <!-- PW Confirmation -->
            <div class="field">
              <label for="password-confirmation">Password (again):
              </label>
              <input type="password" name="password-confirmation" />
            </div>
            <!-- Submit -->
            <input type="submit" value="Sign up" class="field submit" />
          </form>
        </div>

        <!-- Login form -->
        <div class="login">
          <h2>Login</h2>
          <form action="/login" method="post">
            <!-- Username -->
            <div class="field">
              <label for="username">Username:</label>
              <input name="username" />
            </div>
            <!-- Password -->
            <div class="field">
              <label for="password">Password:</label>
              <input type="password" name="password" />
            </div>
            <!-- Submit -->
            <input type="submit" value="Log in" class="field submit" />
          </form>
        </div>
      </section>
    </div>
  </body>
</html>
