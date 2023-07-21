import React from 'react'
import axios from 'axios'
import { useCallback, useEffect, useState } from 'react'
import styled from 'styled-components'
import { Button, Input, Modal, Spacer, Spinner } from 'components'

const CloseButton = styled(({ className, close }) => {
    return <div className={className} onClick={close}>X</div>
})`
    width: 20px;
    height: 20px;
    margin: 6px;
    border: 1px solid white;
    border-radius: 4px;
    position: absolute;
    top: 0;
    right: 0;
    color: white;
    font-family: sans-serif;
    display: flex;
    align-items: center;
    justify-content: center;
    cursor: pointer;
    user-select: none;
    background-color: black;
    &:hover {
        background-color: #111;
    }
`
export default styled(({ className, close, isOpen, setUser }) => {
  const [msg, setMsg] = useState(null)
  const [emailAddress, setEmailAddress] = useState('')
  const [password, setPassword] = useState('')
  const [loading, setLoading] = useState(false)

  const submit = useCallback(() => {
      if (!emailAddress || !password) { return }
      setMsg('')
      setLoading(true);
      axios.post('/api/login', { emailAddress, password }).then(response => {
        setUser(response.data)
        setLoading(false)
        setEmailAddress('')
        setPassword('')
        close()
      }).catch(e => {
        const status = e.response.status
        if (status === 403) {
          setMsg('incorrect credentials')
        } else {
          setMsg(e.message)
        }
        setLoading(false)
      })
  }, [emailAddress, password])

  const keypressHandler = useCallback(evt => {
    if (evt.key === "Enter") {
        submit()
    }
  }, [submit])

  useEffect(() => {
      window.addEventListener('keypress', keypressHandler, false);
      return () => window.removeEventListener('keypress', keypressHandler, false);
  }, [keypressHandler])

  return (
    <Modal isOpen={isOpen}>
      <div className={className}>
      {loading ? (
        <div id="loading">
          <Spinner />
        </div>
      ) : (
        <>
          <CloseButton close={close} />
          <div id="title">Log In</div>
          <Spacer height={20} />
          <div id="inputs">
            <Input
              label="email address"
              value={emailAddress}
              onChange={evt => setEmailAddress(evt.target.value)}
            />
            <Input
              label="password"
              value={password}
              onChange={evt => setPassword(evt.target.value)}
            />
          </div>
          <Spacer height={20} />
          <div id="controls">
            <Button onClick={submit} disabled={!emailAddress || !password}>Submit</Button>
            <Spacer width={10} />
            <Button onClick={close}>Cancel</Button>
          </div>
          {msg && <div id="msg">{msg}</div>}
        </>
      )}
      </div>
    </Modal>
  )
})`
  position: relative;
  width: 500px;
  border: 1px solid white;
  border-radius: 4px;
  padding: 10px;
  display: flex;
  flex-direction: column;
  justify-content: space-between;
  #title {
    font-size: 30px;
  }
  #inputs {
    display: flex;
    flex-direction: column;
  }
  #controls {
    display: flex;
    flex-direction: row;
    justify-content: right;
  }
  #msg {
    position: absolute;
    left: 20px;
    bottom: 20px;
    color: red;
    font-size: 12px;
  }
  #loading {
    height: 100px;
    width: 100%;
    display: flex;
    align-items: center;
    justify-content: center;
  }
`

