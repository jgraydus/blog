import React from 'react'
import { useCallback, useEffect, useRef, useState } from 'react'
import ReactMarkdown from 'react-markdown'
import * as R from 'ramda'
import axios from 'axios'
import remarkGfm from 'remark-gfm'
import styled from 'styled-components'
import { Prism as SyntaxHighlighter } from 'react-syntax-highlighter'
import { dark } from 'react-syntax-highlighter/dist/esm/styles/prism'
import { Button } from 'components'

const Root = styled.div`
  height: 100%;
  width: 100%;
  display: flex;
  flex-flow: column nowrap;
`
const Editor = styled.div`
  padding: 10px;
  flex-grow: 1;
  background-color: black;
  color: white;
  font-family: sans-serif;

  .task-list-item {
    display: flex;
    flex-flow: row wrap;
    align-items: center;
    input {
      display: inline;
      width: 20px;
      height: 20px;
      margin-right: 10px;
    }
  }
`
const Textarea = styled.textarea`
  box-sizing: border-box;
  width: 100%;
  height: 100%;
  resize: none;
  outline: none;
  border: none;
  margin: 0px;
  color: white;
  background-color: black;
`

const computeInsert = (text, cursorPosition, { fileName, fileId }) => {
  const linesRe = /^.*$/gm;
  // divide the text into lines
  const lines = [...text.matchAll(linesRe)]
    .map((match, lineNum) => ({
            lineNum,
            startIndex: match.index,
            endIndex: match.index + match[0].length
         }));

  // find the line which contains the cursor position
  const line = lines
    .find(({ startIndex, endIndex }) =>
              cursorPosition >= startIndex && cursorPosition <= endIndex);

  const isEmpty = line => !line || line.startIndex === line.endIndex;

  // insert at end of current line
  const insertPosition = line.endIndex;
  // add newlines to ensure the inserted str is on its own line and
  // there are blank lines immediately before and after. but don't insert
  // newlines that aren't necessary
  const prefixNewline = isEmpty(line)
    ? (isEmpty(lines[line.lineNum-1]) ? '' : '\n')
    : '\n\n';
  const suffixNewline = isEmpty(lines[line.lineNum+1]) ? '' : '\n';

  return {
    insertPosition,
    str: `${prefixNewline}![${fileName}](/api/files/${fileId})${suffixNewline}`
  }
}

const EditorMode = {
  Preview: 'PREVIEW',
  Write: 'WRITE'
};

export default ({ blogEntryId, editable = true, initialValue, onSave }) => {
  const [markdown, setMarkdown] = useState(initialValue || '');
  const [mode, setMode] = useState(EditorMode.Preview);

  const toggleMode = useCallback(() => {
    if (mode === EditorMode.Preview) {
      setMode(EditorMode.Write);
    } else {
      onSave(markdown);
      setMode(EditorMode.Preview);
    }
  }, [markdown, mode, setMode]);

  // save the state on unmount
  const modeRef = useRef(mode);
  const markdownRef = useRef(markdown);
  useEffect(() => { modeRef.current = mode }, [mode]);
  useEffect(() => { markdownRef.current = markdown }, [markdown]);
  useEffect(() => () => {
    if (modeRef.current === EditorMode.Write) { onSave(markdownRef.current); }
  }, []);

  return (
    <Root>
      <Editor>
        {mode === EditorMode.Preview ? (
          <ReactMarkdown
            remarkPlugins={[remarkGfm]}
            components={{
              code: ({ node, inline, className, children, ...props }) => {
                const match = /language-(\w+)/.exec(className || '');
                return !inline && match ? (
                  <SyntaxHighlighter
                    {...props}
                    children={String(children).replace(/\n$/, '')}
                    style={dark}
                    customStyle={{ backgroundColor: "black", border: "none" }}
                    language={match[1]}
                    PreTag="div"
                  />
                ) : (
                  <code>{children}</code>
                );
              }
            }}
          >
            {markdown}
          </ReactMarkdown>
        ) : (
          <Textarea
            value={markdown}
            onChange={e => setMarkdown(e.target.value)}
            onDragOver={e => {
              // default behavior undoes the dropEffect / effectAlloowed change
              e.preventDefault();
              // required to get a drop event to fire
              e.dataTransfer.dropEffect = "move";
              e.dataTransfer.effectAllowed = "move";
            }}
            onDrop={e => {
              const file = R.path(['dataTransfer', 'files', 0], e);
              if (!file) { return; }

              (async () => {
                 // create a new file record
                 const result = await axios.post(
                   '/api/files',
                   {
                     blogEntryId,
                     name: file.name,
                     mimeType: file.type
                   }
                 );
                 const fileId = R.path(['data', 'fileId'], result);

                 // upload the file
                 await new Promise((resolve, reject) => {
                   const reader = new FileReader();
                   reader.onload = evt => {
                     axios.patch(
                       `/api/files/${fileId}`,
                       evt.target.result,
                       { headers: { 'Content-Type': 'application/octet-stream' } }
                     ).then(resolve);
                   };
                   reader.onabort = reject;
                   reader.onerror = reject;
                   reader.readAsArrayBuffer(file);
                 });

                 // insert an image tag into the markdown text
                 const textarea = e.target;
                 if (!textarea) { return; }
                 textarea.focus();
                 const { insertPosition, str } = computeInsert(
                   textarea.value,
                   textarea.selectionStart,
                   { fileName: file.name, fileId }
                 );
                 textarea.setSelectionRange(insertPosition, insertPosition);
                 document.execCommand('insertText', false, str);

                 setMarkdown(textarea.value);
              })()
            }}
          />
        )}
      </Editor>
      {editable &&
        <Button onClick={toggleMode}>
          {mode === EditorMode.Preview ? "Edit" : "Preview"}
        </Button>}
    </Root>
  );
}

