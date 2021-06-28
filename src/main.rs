use std::{collections::HashMap, io , num::ParseFloatError, fmt};

#[derive(Clone)]
enum RispyExp {
    Symbol(String),
    Number(f64),
    List(Vec<RispyExp>),
    Func(fn(&[RispyExp]) -> Result<RispyExp, RispyErr>),
}

#[derive(Debug)]
enum RispyErr {
    Reason(String),
}

struct RispyEnv {
    data: HashMap<String, RispyExp>,
}

fn tokenize(expr: String) -> Vec<String> {
    expr
        .replace("(", " ( ")
        .replace(")", " ) ")
        .split_whitespace()
        .map(|x| x.to_string())
        .collect()
}

fn parse<'a>(tokens: &'a [String]) -> Result<(RispyExp, &'a [String]), RispyErr> {
    let (token, rest) = tokens.split_first()
        .ok_or(
            RispyErr::Reason("could not get token".to_string())
        )?;
    match &token[..] {
        "(" => read_seq(rest),
        ")" => Err(RispyErr::Reason("unexpected ')'".to_string())),
        _ => Ok((parse_atom(token), rest)),
    }
}

fn read_seq<'a>(tokens: &'a [String]) -> Result<(RispyExp, &'a [String]), RispyErr> {
    let mut res: Vec<RispyExp> = vec![];
    let mut xs = tokens;
    loop {
        let (next_token, rest) = xs
            .split_first()
            .ok_or(RispyErr::Reason("could not find closing ')'".to_string()))
            ?;
        if next_token == ")" {
            return Ok((RispyExp::List(res), rest));
        }
        let (exp, new_xs) = parse(&xs)?;
        res.push(exp);
        xs = new_xs;
    }
}

fn parse_atom(token: &str) -> RispyExp {
    let potential_float: Result<f64, ParseFloatError> = token.parse();
    match potential_float {
        Ok(v) => RispyExp::Number(v),
        Err(_) => RispyExp::Symbol(token.to_string().clone())
    }
}

fn default_env() -> RispyEnv {
    let mut data: HashMap<String, RispyExp> = HashMap::new();
    data.insert(
        "+".to_string(),
        RispyExp::Func(
            |args: &[RispyExp]| -> Result<RispyExp, RispyErr> {
                let sum = parse_list_of_floats(args)?.iter().fold(0.0, |sum, a| sum + a);

                Ok(RispyExp::Number(sum))
            }
        )
    );

    data.insert(
        "-".to_string(),
        RispyExp::Func(
            |args: &[RispyExp]| -> Result<RispyExp, RispyErr> {
                let floats = parse_list_of_floats(args)?;
                let first = *floats.first().ok_or(RispyErr::Reason("expected at least one number".to_string()))?;
                let sum_of_rest = floats[1..].iter().fold(0.0, |sum, a| sum + a);

                Ok(RispyExp::Number(first - sum_of_rest))
            }
        )
    );

    RispyEnv {data}
}

fn parse_list_of_floats(args: &[RispyExp]) -> Result<Vec<f64>, RispyErr> {
    args
        .iter()
        .map(|x| parse_single_float(x))
        .collect()
}

fn parse_single_float(exp: &RispyExp) -> Result<f64, RispyErr> {
    match exp {
        RispyExp::Number(num) => Ok(*num),
        _ => Err(RispyErr::Reason("expected a number".to_string())),
    }
}

fn eval(exp: &RispyExp, env: &mut RispyEnv) -> Result<RispyExp, RispyErr> {
    match exp {
        RispyExp::Symbol(k) => 
            env.data.get(k)
            .ok_or(
                RispyErr::Reason(format!("unexpected symbol k='{}'", k))
            )
        .map(|x| x.clone())
    
        ,
        RispyExp::Number(_a) => Ok(exp.clone()),
        RispyExp::List(list) => {
            let first_form = list
                .first()
                .ok_or(RispyErr::Reason("expected a non-empty list".to_string()))?;
            let arg_forms = &list[1..];
            let first_eval = eval(first_form, env)?;
            match first_eval {
                RispyExp::Func(f) => {
                    let args_eval = arg_forms
                        .iter()
                        .map(|x| eval(x, env))
                        .collect::<Result<Vec<RispyExp>, RispyErr>>();
                    f(&args_eval?)
                },
                _ => Err(
                    RispyErr::Reason("first form must be a function".to_string())
                ),
            }
        },
        RispyExp::Func(_) => Err(
            RispyErr::Reason("unexpected form".to_string())
        ),

    }
}

impl fmt::Display for RispyExp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let str = match self {
            RispyExp::Symbol(s) => s.clone(),
            RispyExp::Number(n) => n.to_string(),
            RispyExp::List(list) => {
                let xs: Vec<String> = list
                    .iter()
                    .map(|x| x.to_string())
                    .collect();
                format!("({})", xs.join(","))
            },
            RispyExp::Func(_) => "Function {}".to_string(),
        };

        write!(f, "{}", str)
    }
}
fn parse_eval(expr: String, env: &mut RispyEnv) -> Result<RispyExp, RispyErr> {
    let (parsed_exp, _) = parse(&tokenize(expr))?;
    let evaled_exp = eval(&parsed_exp, env)?;
    
    Ok(evaled_exp)
  }
  
  fn slurp_expr() -> String {
    let mut expr = String::new();
    
    io::stdin().read_line(&mut expr)
      .expect("Failed to read line");
    
    expr
  }
  
  fn main() {
    let env = &mut default_env();
    loop {
      println!("rispy >");
      let expr = slurp_expr();
      match parse_eval(expr, env) {
        Ok(res) => println!("//  => {}", res),
        Err(e) => match e {
          RispyErr::Reason(msg) => println!("//  => {}", msg),
        },
      }
    }
  }