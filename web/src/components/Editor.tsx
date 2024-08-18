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
          showLineNumbers: true,
          tabSize: 4,
        }}
      />
    </div>
  );
};

export default Editor;
