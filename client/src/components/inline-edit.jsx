import React from 'react'
import { useCallback, useEffect, useRef, useState } from 'react'
import styled from 'styled-components'

const Mode = { Edit: 'EDIT', View: 'VIEW' }

const ViewRoot = styled.div`
    font-size: 18px;
`
const View = ({ onClick, value }) =>
    <ViewRoot onClick={onClick}>{value}</ViewRoot>

const EditRoot = styled.input`
    border: none;
    outline: none;
    height: 30px;
    width: 100%;
    font-size: 18px;
    background-color: #222;
`
const Edit = ({ onSubmit, value }) => {
    const ref = useRef(null);
    const [val, setVal] = useState(value);

    const clickHandler = useCallback(evt => {
        if (ref && ref.current && !ref.current.contains(evt.target)) {
            onSubmit(val);
        }
    }, [ref, onSubmit, val])

    const keypressHandler = useCallback(evt => {
        if (evt.key === "Enter") {
            onSubmit(val);
        }
    }, [onSubmit, val])

    useEffect(() => {
        document.addEventListener('mouseup', clickHandler);
        document.addEventListener('keypress', keypressHandler, false);
        return () => {
            document.removeEventListener('mouseup', clickHandler);
            document.removeEventListener('keypress', keypressHandler, false);
        }
    }, [clickHandler, keypressHandler])

    return (
        <EditRoot
            autoFocus
            ref={ref}
            value={val}
            onClick={evt => evt.stopPropagation()}
            onChange={evt => setVal(evt.target.value)}
        />
    )
}

const InlineEdit = ({ disabled = false, onSubmit, value }) => {
    const [mode, setMode] = useState(Mode.View);

    const setEdit = useCallback(evt => {
        if (!disabled) {
            evt.stopPropagation();
            setMode(Mode.Edit);
        }
    }, [setMode])

    const setView = useCallback(val => {
        onSubmit(val);
        setMode(Mode.View);
    }, [setMode])

    return mode === Mode.View
        ? <View value={value} onClick={setEdit} />
        : <Edit value={value} onSubmit={setView} />
}

export default InlineEdit;

