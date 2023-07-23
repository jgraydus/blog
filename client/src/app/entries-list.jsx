import React from 'react'
import * as R from 'ramda'
import { useNavigate } from 'react-router-dom'
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
const Table = styled.div`
  height: 100%;
  width: 800px;
  .dateColumn { width: 150px; }
  .titleColumn { flex-grow: 1 }
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
const HeaderRow = styled(({ addEntry, className, user }) => (
  <div className={className}>
    <div className="dateColumn">{'Publish date'}</div>
    <div className="titleColumn">{'Title'}</div>
    {user && <span><NewEntryButton user={user} addEntry={addEntry}/></span>}
  </div>
))`
  width: 100%;
  height: 30px;
  display: flex;
  flex-direction: row;
  align-items: center;
`
const Row = styled(({ className, entry }) => {
  const navigate = useNavigate()

  return (
    <div className={className} onClick={() => navigate(`/${entry.blogEntryId}`)}>
      <div className="dateColumn">{new Date(entry.publishDate).toString().substring(0,15)}</div>
      <div className="titleColumn">{entry.title || '--- no title ---'}</div>
    </div>
  )
})`
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
      const entries = R.sortWith([
        R.descend(R.prop('publishDate')),
        R.ascend(R.prop('blogEntryId'))
      ], x.data)
      setEntries(entries)
      setLoading(false)
    })
  }, [user])

  const addEntry = useCallback(entry => {
    setEntries([entry, ...entries])
  }, [entries])

  return (
    <Root>
      {loading
       ? <Spinner />
       : <Table>
           <HeaderRow addEntry={addEntry} user={user}/>
           {R.map(entry => <Row key={entry.blogEntryId} entry={entry} />, entries)}
         </Table>}
    </Root>
  )
}

