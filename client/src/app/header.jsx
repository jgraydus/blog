import React from 'react'
import styled from 'styled-components'
import UserWidget from './user-widget'

export default styled(({ className, openLogInModal }) => (
  <div className={className}>
    <div id="title">grayd.us</div>
    <UserWidget openLogInModal={openLogInModal} />
  </div>
))`
  height: 50px;
  border: 1px solid white;
  padding: 10px;
  display: flex;
  flex-direction: row;
  align-items: center;
  justify-content: space-between;
  #title {
    font-size: 30px;
  }
`

