<html>
  <head>
    <title>Smooch!</title>
  </head>
  <body>

    <h1>Smooch!</h1>

    <!-- demo doll should go here -->

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

    <h2>Upload and play with a KiSS set </h2>

    <p> Eventually this form will be under the user account page, but that's not ready yet.</p>

    <form action="/upload" method="post" enctype="multipart/form-data">

      <input type="file" name="kissfile">
      <input type="submit" value="Upload!">

    </form>

  </body>
</html>
