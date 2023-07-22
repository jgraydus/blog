import React from 'react'
import axios from 'axios'
import { useEffect, useState } from 'react'
import { useParams } from 'react-router-dom'
import { Spinner } from 'components'

export default () => {
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
    <div>{JSON.stringify(entry)}</div>
  )
}

