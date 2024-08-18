import { useEffect, useState } from "react";
import Navbar from "./components/Navbar";
import Editor from "./components/Editor";
import Output from "./components/Output";
import ActionBar from "./components/ActionBar";
import Message from "./components/Message";

const OPTIONAL_BASE_PATH = "/monkeylang";

const App = () => {
  const [code, setCode] = useState<string>('print("Hello, World!")');
  const [output, setOutput] = useState<string>("");
  const [showShareLinkMessage, setShowShareLinkMessage] =
    useState<boolean>(false);

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

  const handleFormat = () => {};
  const handleRun = () => {
    setOutput("TODO: Run code here!");
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
