import React from 'react'
import { useCallback, useState } from 'react'
import ReactMarkdown from 'react-markdown'
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

const EditorMode = {
  Preview: 'PREVIEW',
  Write: 'WRITE'
};

export default ({ onSave, initialValue, editable = true }) => {
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
                  <code {...profile} className={className}>
                    {children}
                  </code>
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

