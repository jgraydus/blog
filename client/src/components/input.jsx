import React from 'react'
import styled from 'styled-components'

const Input = styled.input`
    width: 100%;
    height: 30px;
    border: 1px solid white;
    border-radius: 6px;
    background-color: #111;
    color: #DDD !important;
    &:focus { outline: thin dashed white; }
`
const Label = styled.label`
    width: 100%;
    font-size: 10px;
    line-height: 15px;
    color #BBB;
`
export default ({ label, value, onChange, type = "text" }) => {
    const hasLabel = !!label

    return hasLabel ? (
        <Label>
           <Input value={value} onChange={onChange} type={type} />
           {label} 
        </Label>
    ) : (
        <Input value={value} onChange={onChange} type={type} />
    )
}

