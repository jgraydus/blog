import React from 'react'
import styled from 'styled-components'

export default styled(({ className }) => (
  <div className={className}>&copy; 2023</div>
))`
  height: 50px;
  border: 1px solid white;
  padding: 10px;
  display: flex;
  align-items: center;
`

