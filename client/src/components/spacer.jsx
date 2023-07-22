import React from 'react'
import styled from 'styled-components'

export default styled.div`
  display: inline-block;
  width: ${props => props.width || 0}px;
  height: ${props => props.height || 0}px;
`

