import { useEffect, useState } from "react";
import * as wasm from "wasm";
import url from "wasm/wasm_bg.wasm?url";
import { defaultCode } from "./utils/examples";
import Navbar from "./components/Navbar";
import Editor from "./components/Editor";
import Output from "./components/Output";
import ActionBar from "./components/ActionBar";
import Message from "./components/Message";

const OPTIONAL_BASE_PATH = "/monkeylang";

const App = () => {
  const [code, setCode] = useState<string>(defaultCode);
  const [output, setOutput] = useState<string>("");
  const [showShareLinkMessage, setShowShareLinkMessage] =
    useState<boolean>(false);

  // initialize wasm
  useEffect(() => {
    fetch(url).then((response) =>
      response.arrayBuffer().then((bytes) => {
        wasm.initSync(bytes);
      }),
    );
  }, []);

  // load shared code
  useEffect(() => {
    try {
      const raw = window.location.pathname.split("/").pop();
      if (!raw) return;
      const sharedCode = atob(decodeURIComponent(raw));
      setCode(sharedCode);
    } catch (_) {
      // ignore rigged urls
    }
  }, []);

  const handleFormat = async () => {
    setOutput("");
    try {
      const formattedCode = wasm.format_monkey_code(code);
      setCode(formattedCode);
    } catch (err) {
      setOutput(err as string);
    }
  };

  const handleRun = () => {
    try {
      const result = wasm.eval_monkey_code(code);
      setOutput(result);
    } catch (err) {
      setOutput(err as string);
    }
  };

  const handleShare = () => {
    let baseUrl = window.location.origin;
    if (window.location.pathname.startsWith(OPTIONAL_BASE_PATH)) {
      baseUrl += OPTIONAL_BASE_PATH;
    }
    const encodedCode = encodeURIComponent(btoa(code));
    const link = `${baseUrl}/${encodedCode}`;
    navigator.clipboard.writeText(link);
    setShowShareLinkMessage(true);
  };

  const handleExamples = () => {};

  return (
    <div className="flex flex-col h-screen">
      <Navbar />
      <div className="flex-1 flex flex-col gap-3 p-4">
        <Message
          open={showShareLinkMessage}
          message="Saved share link to the clipboard!"
          close={() => setShowShareLinkMessage(false)}
        />
        <ActionBar
          handleRun={handleRun}
          handleFormat={handleFormat}
          handleShare={handleShare}
          handleExamples={handleExamples}
        />
        <Editor code={code} setCode={setCode} />
        <Output output={output} handleClose={() => setOutput("")} />
      </div>
    </div>
  );
};

export default App;
