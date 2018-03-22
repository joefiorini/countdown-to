open Lwt;
open Lwt.Infix;

module C = Cohttp_lwt_unix;

open Graphql_lwt;
open ISO8601.Permissive;

type role =
  | User
  | Admin;

module Countdown = {
  type t = {
    short: string,
    description: string,
    startDate: float,
    endDate: float
  };

  let t = {
    let encode = ({short, description, startDate, endDate}) =>
      [@implicit_arity] Ok(short, description, startDate, endDate);
    let decode = ((short, description, startDate, endDate)) => Ok({short, description, startDate, endDate});
    let rep = Caqti_type.(tup4(string, string, float, float));
    Caqti_type.custom(~encode, ~decode, rep);
  }
};

module Q = {
  let create_timer =
    Caqti_request.exec(
      Caqti_type.(tup4(string, string, float, float)),
      "INSERT INTO timers (short, description, starts_on, ends_on)"
    );
  let lookup_timer = Caqti_request.find_opt(
    Caqti_type.string,
    Countdown.t,
    "SELECT short, description, starts_on, ends_on FROM timers WHERE short = ?"
  );
};

let create_timer = ((module Db): (module Caqti_lwt.CONNECTION), short, description, startDate, endDate) => Db.exec(Q.create_timer, (short, description, startDate, endDate));
let lookup_timer = (short, (module Db): (module Caqti_lwt.CONNECTION)) =>
  Db.find_opt(Q.lookup_timer, short);

let countdown =
  Schema.(
    obj("countdown", ~fields=(_) =>
      [
        field("short", ~args=Arg.([]), ~typ=non_null(string), ~resolve=((), p: Countdown.t) =>
          p.short
        ),
        field(
          "description",
          ~args=Arg.([]),
          ~typ=non_null(string),
          ~resolve=((), p: Countdown.t) =>
          p.description
        ),
        field(
          "startDate", ~args=Arg.([]), ~typ=non_null(string), ~resolve=((), p: Countdown.t) =>
          string_of_datetime(p.startDate)
        ),
        field(
          "endDate",
          ~args=Arg.([]),
          ~typ=non_null(string),
          ~resolve=((), p: Countdown.t) =>
          string_of_datetime(p.endDate)
        )
      ]
    )
);

let (>>=?) = (m, f) =>
  m
  >>= (
    fun
    | Ok(x) => f(x)
    | Error(err) => Lwt.return(Error(err))
  );

let testCountdown: Countdown.t =
  { short: "blah", description: "Diddy doo dah", startDate: datetime("2018-03-24T12:00:00Z"), endDate: datetime("2018-03-30T12:00:00Z")};
let schema =
  Schema.(
    schema([
      io_field(
        "countdown",
        ~args=Arg.[arg("short", ~typ=non_null(string))],
        ~typ=countdown,
        ~resolve=((), db, short) =>
        lookup_timer(short, db) >>= (fun
          | Ok(timer_opt) => Lwt.return(timer_opt)
          | Error(err) => Lwt.return(None)
          )
      ),
    ],
    ~mutations=[
      field("createCountdown",
        ~typ=non_null(countdown),
        ~args=Arg.[
          arg("short", ~typ=string),
          arg("description", ~typ=non_null( string)),
          arg("startDate", ~typ=non_null( string)),
          arg("endDate", ~typ=non_null( string)),
        ],
        ~resolve=(_, _, _, _, _, _ ) => testCountdown
      )
    ])
  );

let db = Caqti_lwt.connect("postgres://localhost/countdown-to_dev");
let () = Server.start(~port=4000, ~ctx=() => db, schema) |> Lwt_main.run;
