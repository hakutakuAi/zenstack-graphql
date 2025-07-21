import { expect, test, describe, beforeAll } from 'bun:test'
import { existsSync, readFileSync } from 'fs'
import { join } from 'path'
import { SchemaComposer } from 'graphql-compose'

describe('Basic Example', () => {
    const schemaPath = join(__dirname, './schema.graphql')
    const schemaComposer = new SchemaComposer()

    beforeAll(() => {
        expect(existsSync(schemaPath)).toBe(true)
        schemaComposer.add(readFileSync(schemaPath, 'utf8'))
    })

    test('Schema contains all expected object types', () => {
        const expectedTypes = ['Author', 'Book', 'Review', 'Article', 'Publisher']

        for (const typeName of expectedTypes) {
            expect(schemaComposer.has(typeName)).toBe(true)
            expect(schemaComposer.getOTC(typeName)).toBeTruthy()
        }
    })

    test('Object types have expected fields', () => {
        const authorType = schemaComposer.getOTC('Author')
        expect(authorType.hasField('id')).toBe(true)
        expect(authorType.hasField('name')).toBe(true)
        expect(authorType.hasField('books')).toBe(true)
        expect(authorType.getFieldType('books').toString()).toBe('[Book!]!')

        const bookType = schemaComposer.getOTC('Book')
        expect(bookType.hasField('title')).toBe(true)
        expect(bookType.hasField('category')).toBe(true)
        expect(bookType.hasField('author')).toBe(true)
        expect(bookType.getFieldType('author').toString()).toBe('Author!')

        const reviewType = schemaComposer.getOTC('Review')
        expect(reviewType.hasField('rating')).toBe(true)
        expect(reviewType.hasField('book')).toBe(true)
        expect(reviewType.getFieldType('book').toString()).toBe('Book!')
    })

    test('Enum types are properly defined', () => {
        expect(schemaComposer.has('BookCategory')).toBe(true)
        const categoryEnum = schemaComposer.getETC('BookCategory')

        const enumValues = categoryEnum.getFields()
        expect(Object.keys(enumValues)).toContain('FICTION')
        expect(Object.keys(enumValues)).toContain('NONFICTION')
        expect(Object.keys(enumValues)).toContain('SCIENCE')
    })

    test('Scalar types are properly defined', () => {
        expect(schemaComposer.has('DateTime')).toBe(true)
        expect(schemaComposer.has('Decimal')).toBe(true)
        expect(schemaComposer.getSTC('DateTime')).toBeTruthy()
        expect(schemaComposer.getSTC('Decimal')).toBeTruthy()
    })

    test('Relation fields are properly defined', () => {
        const authorType = schemaComposer.getOTC('Author')
        expect(authorType.getFieldType('books').toString()).toBe('[Book!]!')
        expect(authorType.getFieldType('articles').toString()).toBe('[Article!]!')

        const bookType = schemaComposer.getOTC('Book')
        expect(bookType.getFieldType('author').toString()).toBe('Author!')
        expect(bookType.getFieldType('reviews').toString()).toBe('[Review!]!')

        const articleType = schemaComposer.getOTC('Article')
        expect(articleType.getFieldType('author').toString()).toBe('Author!')
    })

    test('All types and fields in schema are valid', () => {
        const schema = schemaComposer.buildSchema()
        expect(schema).toBeTruthy()
    })
})
