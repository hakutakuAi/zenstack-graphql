import { expect, test, describe, beforeAll } from 'bun:test'
import { existsSync, readFileSync } from 'fs'
import { join } from 'path'

describe('TypeGraphQL Test Example', () => {
	const schemaPath = join(__dirname, './schema.ts')
	let schemaContent: string

	beforeAll(() => {
		expect(existsSync(schemaPath)).toBe(true)
		schemaContent = readFileSync(schemaPath, 'utf8')
	})

	test('Generated TypeScript file contains imports', () => {
		expect(schemaContent).toContain('import { ObjectType, Field, ID, Int, Float, registerEnumType, InputType, ArgsType } from "type-graphql"')
		expect(schemaContent).toContain('import { GraphQLJSON } from "graphql-scalars"')
		expect(schemaContent).toContain('import "reflect-metadata"')
	})

	test('Generated file contains User class with ObjectType decorator', () => {
		expect(schemaContent).toContain('@ObjectType()')
		expect(schemaContent).toContain('export class User')
		expect(schemaContent).toContain('@Field(() => String)')
		expect(schemaContent).toContain('id!: string')
		expect(schemaContent).toContain('name!: string')
		expect(schemaContent).toContain('email!: string')
	})

	test('Generated file contains Post class with ObjectType decorator', () => {
		expect(schemaContent).toContain('export class Post')
		expect(schemaContent).toContain('title!: string')
		expect(schemaContent).toContain('content!: string')
		expect(schemaContent).toContain('published!: boolean')
	})

	test('Generated file contains Status enum', () => {
		expect(schemaContent).toContain('export enum Status')
		expect(schemaContent).toContain('DRAFT = "DRAFT"')
		expect(schemaContent).toContain('PUBLISHED = "PUBLISHED"')
		expect(schemaContent).toContain('ARCHIVED = "ARCHIVED"')
		expect(schemaContent).toContain('registerEnumType(Status')
	})

	test('Generated file contains proper Field decorators', () => {
		expect(schemaContent).toContain('@Field(() => String)')
		expect(schemaContent).toContain('@Field(() => Boolean)')
		expect(schemaContent).toContain('@Field(() => Date)')
	})

	test('Generated file handles optional fields', () => {
		expect(schemaContent).toContain('nullable: true')
		expect(schemaContent).toContain('bio?: string')
	})

	test('Generated file has proper property assertions', () => {
		expect(schemaContent).toContain('id!: string')
		expect(schemaContent).toContain('name!: string')
		expect(schemaContent).toContain('email!: string')
	})

	test('Generated TypeScript code is syntactically valid', () => {
		const openBraces = (schemaContent.match(/\{/g) || []).length
		const closeBraces = (schemaContent.match(/\}/g) || []).length
		expect(openBraces).toBe(closeBraces)

		const openParens = (schemaContent.match(/\(/g) || []).length
		const closeParens = (schemaContent.match(/\)/g) || []).length
		expect(openParens).toBe(closeParens)
	})
})
