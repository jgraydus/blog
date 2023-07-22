import React from 'react'
import * as R from 'ramda'
import axios from 'axios'
import { useCallback, useEffect, useState } from 'react'
import styled from 'styled-components'
import { Button, Spacer, Spinner } from 'components'

const Root = styled.div`
  height: 100%;
  width: 100%;
  display: flex;
  align-items: center;
  justify-content: center;
`
const Content = styled.div`
  height: 100%;
  width: 50%;
`
const NewEntryButton = styled(({ addEntry, className, user }) => {
  return (
    <div className={className}>
      <Button
        onClick={() => {
          axios.post('/api/entries').then(x => addEntry(x.data))
        }}
      >Create New Entry</Button> 
    </div>
  )
})`
  width: 100%;
  display: flex;
  justify-content: center;
`

const Row = styled(({ className, entry }) => (
    <div className={className}>
        <div>{new Date(entry.publishDate).toString().substring(0,15)}</div>
        <Spacer width={20} />
        <div>{entry.title || '--- no title ---'}</div>
    </div>
))`
  width: 100%;
  height: 30px;
  display: flex;
  flex-direction: row;
  align-items: center;
  cursor: pointer;
  &:hover {
    background-color: #222; 
  }
`

export default ({ user }) => {
  const [loading, setLoading] = useState(true)
  const [entries, setEntries] = useState(null)

  useEffect(() => {
    axios.get('/api/entries').then(x => {
      const entries = x.data
      setEntries(entries)
      setLoading(false)
    })
  }, [])

  const addEntry = useCallback(entry => {
    setEntries([...entries, entry])
  }, [entries])

  return (
    <Root>
      {loading
       ? <Spinner />
       : <Content>
           {R.map(entry => <Row key={entry.blogEntryId} entry={entry} />, entries)}
           {user && <NewEntryButton user={user} addEntry={addEntry}/>}
         </Content>}
    </Root>
  )
}

