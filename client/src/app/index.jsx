import React from 'react'
import axios from 'axios'
import { useEffect, useState } from 'react'
import { Route, Routes } from 'react-router-dom'
import styled from 'styled-components'
import { Spinner } from 'components'
import Entry from './entry'
import EntriesList from './entries-list'
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
  const [loading, setLoading] = useState(true)
  const [logInModalIsOpen, setLogInModalIsOpen] = useState(false)
  const [user, setUser] = useState(null)

  useEffect(() => {
    // if we already have the auth token cookie, then we can just get the user data
    axios.post('/api/me').then(x => {
      setUser(x.data)
      setLoading(false)
    }).catch(e => setLoading(false));
  }, []);

  if (loading) {
    return (
      <Viewport>
        <Spinner />
      </Viewport>
    )
  }

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
        <Routes>
          <Route path="/" element={<EntriesList user={user} />} />
          <Route path=":blogEntryId" element={<Entry user={user} />} />
        </Routes>
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

