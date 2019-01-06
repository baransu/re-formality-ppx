module Form = [%formality {|
  name: string
|}];

let component = ReasonReact.statelessComponent(__MODULE__);
let make = _ => {
  ...component,
  render: _ => <div> {ReasonReact.string("Hello world")} </div>,
};