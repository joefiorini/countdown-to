open Lwt;

open Cohttp;

open Cohttp_lwt;

open Cohttp_lwt_unix;

let server = {
  let callback = (_conn, req, body) =>
    body
    |> Cohttp_lwt.Body.to_string
    >|= (body => "Hello world")
    >>= (body => Server.respond_string(~status=`OK, ~body, ()));
  Server.create(~mode=`TCP(`Port(4000)), Server.make(~callback, ()));
};

let () = ignore(Lwt_main.run(server));
