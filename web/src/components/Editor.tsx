import AceEditor from "react-ace";

const Editor = () => {
  const code = "var message = 'Monaco Editor!' \nconsole.log(message);";
  return (
    <div className="rounded-lg border flex-1 flex flex-col">
      <AceEditor
        width="100%"
        height="100%"
        value={code}
        fontSize="14px"
        highlightActiveLine={true}
        setOptions={{
          enableLiveAutocompletion: false,
          showLineNumbers: true,
          tabSize: 2,
        }}
      />
    </div>
  );
};

export default Editor;
