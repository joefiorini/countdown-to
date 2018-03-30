open Lwt;

open Lwt.Infix;

module C = Cohttp_lwt_unix;

open Graphql_lwt;

open ISO8601.Permissive;

open Lib;

type role =
  | User
  | Admin;

module Countdown = {
  type t = {
    short: string,
    description: string,
    startDate: string,
    endDate: string
  };
  exception Date_Format_Error(string);
  open Caqti_prereq;
  open Printf;
  let ptime_of_iso8601 = s => {
    let [s1, s2] = String.split_on_char('T', s);
    Ptime.of_date_time((
      (
        int_of_string(String.sub(s1, 0, 4)),
        int_of_string(String.sub(s1, 5, 2)),
        int_of_string(String.sub(s1, 8, 2))
      ),
      (
        (
          int_of_string(String.sub(s2, 0, 2)),
          int_of_string(String.sub(s2, 4, 2)),
          int_of_string(String.sub(s2, 7, 2))
        ),
        0
      )
    ));
  };
  let iso8601_of_ptime = (((y, m, d), ((hh, mm, ss), _))) =>
    sprintf("%04d-%02d-%02dT%02d:%02d:%02dZ", y, m, d, hh, mm, ss);
  let date_or_error = date =>
    ptime_of_iso8601(date)
    |> (
      fun
      | Some(d) => d
      | None => raise(Date_Format_Error("Unable to get date"))
    );
  /* switch (ptime_of_iso8601(date)) {
     | Ok((d, t)) => d
     | Error(e) => raise(Date_Format_Error(e))
     }; */
  let t = {
    let encode = ({short, description, startDate, endDate}) =>
      [@implicit_arity]
      Ok(short, description, date_or_error(startDate), date_or_error(endDate));
    let decode = ((short, description, startDate, endDate)) =>
      Ok({
        short,
        description,
        startDate: iso8601_of_ptime(Ptime.to_date_time(startDate)),
        endDate: iso8601_of_ptime(Ptime.to_date_time(endDate))
      });
    let rep =
      Caqti_type.(tup4(string, string, Caqti_type.ptime, Caqti_type.ptime));
    Caqti_type.custom(~encode, ~decode, rep);
  };
};

module Q = {
  let create_timer =
    Caqti_request.find(
      Caqti_type.(tup4(string, string, string, string)),
      Countdown.t,
      "INSERT INTO timers (short, description, starts_on, ends_on) values (?,?,?,?) returning short, description, starts_on, ends_on"
    );
  let lookup_timer =
    Caqti_request.find_opt(
      Caqti_type.string,
      Countdown.t,
      "SELECT short, description, starts_on, ends_on FROM timers WHERE short = ?"
    );
};

let create_timer =
    (
      short,
      description,
      startDate,
      endDate,
      (module Db): (module Caqti_lwt.CONNECTION)
    ) =>
  Db.find(Q.create_timer, (short, description, startDate, endDate));

let lookup_timer = (short, (module Db): (module Caqti_lwt.CONNECTION)) =>
  Db.find_opt(Q.lookup_timer, short);

let countdown =
  Schema.(
    obj("countdown", ~fields=db =>
      [
        field(
          "short",
          ~args=Arg.([]),
          ~typ=non_null(string),
          ~resolve=(db, p: Countdown.t) =>
          p.short
        ),
        field(
          "description",
          ~args=Arg.([]),
          ~typ=non_null(string),
          ~resolve=(db, p: Countdown.t) =>
          p.description
        ),
        field(
          "startDate",
          ~args=Arg.([]),
          ~typ=non_null(string),
          ~resolve=(db, p: Countdown.t) =>
          p.startDate
        ),
        field(
          "endDate",
          ~args=Arg.([]),
          ~typ=non_null(string),
          ~resolve=(db, p: Countdown.t) =>
          p.endDate
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

let testCountdown: Countdown.t = {
  short: "blah",
  description: "Diddy doo dah",
  startDate: "2018-03-24T12:00:00Z",
  endDate: "2018-03-30T12:00:00Z"
};

exception Lookup_Error(Caqti_error.call_or_retrieve);

let schema =
  Schema.(
    schema(
      [
        io_field(
          "countdown",
          ~args=Arg.[arg("short", ~typ=non_null(string))],
          ~typ=countdown,
          ~resolve=(db, (), short) =>
          db
          >>= lookup_timer(short)
          >|= (
            fun
            | Ok(timer_opt) => timer_opt
            | Error(err) => {
                Lwt_io.printf("Error: %s\n", Caqti_error.show(err));
                raise(Lookup_Error(err));
              }
          )
        )
      ],
      ~mutations=[
        io_field(
          "createCountdown",
          ~typ=non_null(countdown),
          ~args=
            Arg.[
              arg("short", ~typ=string),
              arg("description", ~typ=non_null(string)),
              arg("startDate", ~typ=non_null(string)),
              arg("endDate", ~typ=non_null(string))
            ],
          ~resolve=(db, _, short, description, startDate, endDate) => {
            let resolvedShort =
              switch short {
              | Some(s) => s
              | None => RandomWord.get()
              };
            db
            >>= create_timer(resolvedShort, description, startDate, endDate)
            >|= (
              fun
              | Ok(timer) => timer
              | Error(err) => {
                  Lwt_io.printf("Error: %s\n", Caqti_error.show(err));
                  raise(Lookup_Error(err));
                }
            );
          }
        )
      ]
    )
  );

let db =
  Caqti_lwt.connect(Uri.of_string("postgresql://localhost/countdown-to_dev"))
  >>= Caqti_lwt.or_fail;

let () = Server.start(~port=4000, ~ctx=() => db, schema) |> Lwt_main.run;