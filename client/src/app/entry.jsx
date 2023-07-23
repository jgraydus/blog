import React from 'react'
import axios from 'axios'
import { useCallback, useEffect, useState } from 'react'
import { useNavigate, useParams } from 'react-router-dom'
import styled from 'styled-components'
import { Button, InlineEdit, MdEditor, Spacer, Spinner } from 'components'

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

  const changeTitle = useCallback(title => {
    setEntry({ ...entry, title })
    axios.patch(`/api/entries/${entry.blogEntryId}`, { title })
  }, [entry])

  const changeContent = useCallback(content => {
    setEntry({ ...entry, content })
    axios.patch(`/api/entries/${entry.blogEntryId}`, { content })
  }, [entry])

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
          <InlineEdit
            onSubmit={changeTitle}
            value={entry.title}
            placeholder={'--- no title ---'}
          />
        </span>
        {user && <PublishButton entry={entry}/>}
      </div>
      <div id="content">
        <MdEditor
          onSave={changeContent}
          initialValue={entry.content}
          editable={!!user}
         />
      </div>
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
    border-bottom: 0px;
  }
`

