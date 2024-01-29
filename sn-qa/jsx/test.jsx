// Simplest component: stateless function.
function Hello(props) {
    return (
      <div className="txt-bold">
        Hello {props.name}
      </div>
    );
  }
  
  // Without JSX.
  function Hello(props) {
    return React.createElement(
      'div', 
      { className: 'txt-bold' }, 
      `Hello ${props.name}`
    );
  }
  
  // As a class.
  class Hello extends React.Component {
    render() {
      return (
        <div>
          Hello {this.props.name}
        </div>
      );
    }
  }
  
  // Todo app.
  // Taken from https://reactjs.org/. Look there for the interactive example.
  class TodoApp extends React.Component {
    constructor(props) {
      super(props);
      this.state = { items: [], text: '' };
      this.handleChange = this.handleChange.bind(this);
      this.handleSubmit = this.handleSubmit.bind(this);
    }
  
    render() {
      return (
        <div>
          <h3>TODO</h3>
          <TodoList items={this.state.items} />
          <form onSubmit={this.handleSubmit}>
            <label htmlFor="new-todo">
              What needs to be done?
            </label>
            <input
              id="new-todo"
              onChange={this.handleChange}
              value={this.state.text}
            />
            <button>
              Add #{this.state.items.length + 1}
            </button>
          </form>
        </div>
      );
    }
  
    handleChange(e) {
      this.setState({ text: e.target.value });
    }
  
    handleSubmit(e) {
      e.preventDefault();
      if (!this.state.text.length) {
        return;
      }
      const newItem = {
        text: this.state.text,
        id: Date.now()
      };
      this.setState(prevState => ({
        items: prevState.items.concat(newItem),
        text: ''
      }));
    }
  }
  
  class TodoList extends React.Component {
    render() {
      const items = this.props.items.map(item => (
        <li key={item.id}>{item.text}</li>
      ));
      
      return (
        <ul>
          {items}
        </ul>
      );
    }
  }