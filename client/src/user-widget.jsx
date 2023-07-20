import React from 'react'
import styled from 'styled-components'
import Spacer from './spacer.jsx'

const Root = styled.div`
    height: 4opx;
    display: flex;
    flex-direction: row;
    flex-wrap: nowrap;
    font-size: 16px;
`

export default () => {

  return (
    <Root>
      <div>{'Log in'}</div>
      <Spacer width={20} />
      <div>{'Register'}</div>
    </Root>
  )
}

