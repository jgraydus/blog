import React from 'react'
import styled from 'styled-components'
import { Button, Input, Modal, Spacer } from 'components'

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
export default styled(({ className, close, isOpen }) => {
  return (
    <Modal isOpen={isOpen}>
      <div className={className}>
        <CloseButton close={close} />
        <div id="title">Log In</div>
        <Spacer height={20} />
        <div id="inputs">
          <Input label="email address" />
          <Input label="password" />
        </div>
        <Spacer height={20} />
        <div id="controls">
          <Button>Submit</Button>
          <Spacer width={10} />
          <Button onClick={close}>Cancel</Button>
        </div>
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
`

