import React from 'react'
import axios from 'axios'
import { useState } from 'react'
import styled from 'styled-components'
import Footer from './footer'
import Header from './header'
import LogInModal from './login-modal'
import UserWidget from './user-widget'
import Viewport from './viewport'

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
const Content = styled(({ className, children }) => (
  <div className={className}>{children}</div>
))`
  height: 100%;
  width: 100%;
  padding: 10px;
`
export default () => {
  const [logInModalIsOpen, setLogInModalIsOpen] = useState(false)
  const [user, setUser] = useState(null)

  return (
    <Viewport>
      <Header
        logOut={() => {
          axios.post('/api/logout')
          setUser(null)
        }}
        openLogInModal={() => setLogInModalIsOpen(true)}
        user={user}
      />
      <Content>
        {/* TODO */}
        <LogInModal
          close={() => setLogInModalIsOpen(false)}
          isOpen={logInModalIsOpen}
          setUser={setUser}
        />
      </Content>
      <Footer />
    </Viewport>
  )
};

