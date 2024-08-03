import Navbar from "./components/Navbar";
import Editor from "./components/Editor";
import Output from "./components/Output";

const App = () => {
  return (
    <div className="flex flex-col h-screen">
      <Navbar />
      <div className="flex-1 flex flex-col gap-6 p-6">
        <Editor />
        <Output />
      </div>
    </div>
  );
};

export default App;
