import React from 'react'
import MarkdownEditor from './md-editor'
import styled from 'styled-components'
import UserWidget from './user-widget'

const Root = styled.div`
  margin: 0px;
  padding: 0px;
  height: 100vh;
  width: 100%;
  display: flex;
  flex-direction: column;
  background-color: black;
  color: white;
  font-family: sans-serif;
`
const Header = styled(({ className }) => (
  <div className={className}>
    <div>grayd.us</div>
    <UserWidget />
  </div>
))`
  height: 50px;
  border: 1px solid white;
  padding: 10px;
  display: flex;
  flex-direction: row;
  align-items: center;
  justify-content: space-between;
  font-size: 30px;
`
const Content = styled(({ className, children }) => (
  <div className={className}>{children}</div>
))`
  height: 100%;
  width: 100%;
  padding: 10px;
`
const Footer = styled(({ className }) => (
  <div className={className}>footer</div>
))`
  height: 50px;
  border: 1px solid white;
  padding: 10px;
  display: flex;
  align-items: center;
`

export default () => (
  <Root>
    <Header />
    <Content>
      {"hello"}
    </Content>
    <Footer />
  </Root>
);


// <MarkdownEditor onSave={console.log} />


