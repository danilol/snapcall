const jsonServer = require("json-server")
const server = jsonServer.create()
const router = jsonServer.router("db.json")
const middlewares = jsonServer.defaults()
const port = process.env.PORT || 3000

server.use(middlewares)

server.use(function(req, res, next){
  setTimeout(next, 3000);
});

server.use(jsonServer.bodyParser)
server.use((req, res, next) => {
  if (req.method === "POST") {

    // simulate an error
    if (req.body.technology === "AnyOther") {
      res.status(500).jsonp({
        error: "Invalid screen sharing technology error!"
      })
      return;
    }
  }
  next()
})

server.use(router)

server.listen(port, () => {
  console.log("JSON Server is running on http://localhost:" + port)
})
