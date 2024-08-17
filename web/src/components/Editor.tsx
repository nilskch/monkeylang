import AceEditor from "react-ace";

type EditorProps = {
  code: string;
  setCode: (code: string) => void;
};

const Editor = ({ code, setCode }: EditorProps) => {
  return (
    <div className="rounded-lg border flex-1 flex flex-col">
      <AceEditor
        width="100%"
        height="100%"
        value={code}
        onChange={setCode}
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
