open Lwt;

module C = Cohttp_lwt_unix;

open Graphql_lwt;
open ISO8601.Permissive;

type role =
  | User
  | Admin;

type countdown = {
  short: string,
  description: string,
  startDate: float,
  endDate: float
};

let countdown =
  Schema.(
    obj("countdown", ~fields=(_) =>
      [
        field("short", ~args=Arg.([]), ~typ=non_null(string), ~resolve=((), p) =>
          p.short
        ),
        field(
          "description",
          ~args=Arg.([]),
          ~typ=non_null(string),
          ~resolve=((), p) =>
          p.description
        ),
        field(
          "startDate", ~args=Arg.([]), ~typ=non_null(string), ~resolve=((), p) =>
          string_of_datetime(p.startDate)
        ),
        field(
          "endDate",
          ~args=Arg.([]),
          ~typ=non_null(string),
          ~resolve=((), p) =>
          string_of_datetime(p.endDate)
        )
      ]
    )
  );
let testCountdown =
  { short: "blah", description: "Diddy doo dah", startDate: datetime("2018-03-24T12:00:00Z"), endDate: datetime("2018-03-30T12:00:00Z")};
let schema =
  Schema.(
    schema([
      io_field(
        "countdown",
        ~args=Arg.[arg("short", ~typ=non_null(string))],
        ~typ=countdown,
        ~resolve=((), (), short) =>
        Lwt.return(Some(testCountdown))
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

let () = Server.start(~port=4000, ~ctx=() => (), schema) |> Lwt_main.run;
