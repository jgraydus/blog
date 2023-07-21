import React from 'react'
import styled from 'styled-components'
import Spacer from 'components/spacer.jsx'

const Root = styled.div`
    height: 4opx;
    display: flex;
    flex-direction: row;
    flex-wrap: nowrap;
    font-size: 16px;
    #link {
      cursor: pointer;
      &:hover { color: #AAA; }
    }
`
export default ({ logOut, openLogInModal, user }) => {
  return (
    <Root>
    {!!user
      ? <div id="link" onClick={logOut}>{'Log Out'}</div>
      : <div id="link" onClick={openLogInModal}>{'Log In'}</div>}
    </Root>
  )
}

