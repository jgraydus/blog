import React from 'react'
import styled from 'styled-components'

// this is meant to be the outer-most div. it sets global properties
const Viewport = styled.div`
  background-color: black;
  box-sizing: border-box;

  position: absolute;
  left: 0;
  right: 0;
  top: 0;
  bottom: 0;

  display: flex;
  flex-direction: column;

  * {
      box-sizing: border-box;
      color: #EEE;
      font-family: sans-serif;

      a {
          color: #BBB;
          text-decoration: none;
      }

      a:hover {
          color: #FFF;
          cursor: pointer;
      }
    }
`

export default Viewport

