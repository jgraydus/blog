import React from 'react'
import Modal from 'react-modal'
import styled from 'styled-components'

const modalStyles = {
    overlay: {
        backgroundColor: 'rgba(0, 0, 0, 0.7)'
    },
    content: {
        backgroundColor: 'rgba(0, 0, 0, 0)',
        border: 'none',
        padding: 'none',
        outline: 'none',
        overflow: 'hidden',
    }
}

const Content = styled.div`
    width: 100%;
    height: 100%;
    display: flex;
    align-items: center;
    justify-content: center;

    // the following styles are set for Viewport which
    // wraps the entire app. however, the modal gets attached
    // to the body element. I haven't figured out a better way to
    // do this yet, so I'm just duplicating these here
    * {
        box-sizing: border-box;
        color: #EEE;
        font-family: sans-serif;

        a {
            color: #BBB;
            text-decoration: none;
        }

        a:hover {
            color: #FFF;
            cursor: pointer;
        }
    }
`

export default ({ children, isOpen }) =>
    <Modal isOpen={isOpen} style={modalStyles}>
        <Content>{children}</Content>
    </Modal>

