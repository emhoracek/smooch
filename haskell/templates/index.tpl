<html>
  <head>
    <title>Smooch!</title>
  </head>
  <body>

    <h1>Smooch!</h1>

    <!-- demo doll -->

    <!-- create your account! -->

    <form action="/users/create" method="post">
      <div>
        <label for="username">
          Username: <input name="username" />
        </label>
      </div>
      <div>
        <label for="email">
          Email: <input name="email" />
        </label>
      </div>
      <div>
        <label for="password">
          Password: <input name="password" />
        </label>
      </div>
      <div>
        <label for="password-confirmation">Password (again):
          <input name="password-confirmation" />
        </label>
      </div>

      <input type="submit" />
    </form>
  </body>
</html>
