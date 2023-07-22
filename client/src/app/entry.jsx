import React from 'react'
import axios from 'axios'
import { useEffect, useState } from 'react'
import { useNavigate, useParams } from 'react-router-dom'
import styled from 'styled-components'
import { Button, InlineEdit, Spacer, Spinner } from 'components'

const PublishButton = ({ entry }) => <Button>{entry.isPublished ? 'unpublish' : 'publish'}</Button>

export default styled(({ className, user }) => {
  const navigate = useNavigate()
  const { blogEntryId } = useParams()
  const [loading, setLoading] = useState(true)
  const [entry, setEntry] = useState(null)

  useEffect(() => {
    axios.get(`/api/entries/${blogEntryId}`).then(x => {
      setEntry(x.data)
      setLoading(false)
    })
  }, [])

  if (loading) {
    return <Spinner />
  }

  return (
    <div className={className}>
      <div id="header">
        <Button onClick={() => navigate('/')}>back</Button>
        <Spacer width={20} />
        <span id="date">{new Date(entry.publishDate).toString().substring(0, 15)}</span>
        <Spacer width={20} />
        <span id="title">
          <InlineEdit onSubmit={() => {}} value={entry.title || '--- no title ---'} />
        </span>
        {user && <PublishButton entry={entry}/>}
      </div>
      <div id="content">{entry.content}</div>
    </div>
  )
})`
  height: 100%;
  width: 100%;
  display: flex;
  flex-direction: column;
  #date {
    white-space: nowrap;
  }
  #header {
    height: 42px;
    font-size: 20px;
    color: white;
    display: flex;
    flex-direction: row;
    align-items: center;
  }
  #title {
    flex-grow: 1;
  }
  #content {
    height: 100%;
    width: 100%;
    border: 1px solid white;
  }
`

