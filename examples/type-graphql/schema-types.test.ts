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

	test('Generated file contains Category and Comment classes', () => {
		expect(schemaContent).toContain('export class Category')
		expect(schemaContent).toContain('export class Comment')
		expect(schemaContent).toContain('export class PostCategory')
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

	// Filter Input Types Tests
	describe('Filter Input Types', () => {
		test('Contains base filter input types', () => {
			expect(schemaContent).toContain('export class NumericFilterInput')
			expect(schemaContent).toContain('export class DateTimeFilterInput')
			expect(schemaContent).toContain('export class StringFilterInput')
			expect(schemaContent).toContain('export class BooleanFilterInput')
		})

		test('Contains model-specific filter input types', () => {
			expect(schemaContent).toContain('export class UserFilterInput')
			expect(schemaContent).toContain('export class PostFilterInput')
			expect(schemaContent).toContain('export class CategoryFilterInput')
			expect(schemaContent).toContain('export class PostCategoryFilterInput')
			expect(schemaContent).toContain('export class CommentFilterInput')
		})

		test('Filter inputs have proper InputType decorators', () => {
			const filterInputMatches = schemaContent.match(/@InputType\(\)\s*\n\s*export class \w+FilterInput/g)
			expect(filterInputMatches).not.toBeNull()
			expect(filterInputMatches!.length).toBeGreaterThan(4)
		})

		test('StringFilterInput has correct fields and types', () => {
			expect(schemaContent).toContain('equals?: string | undefined')
			expect(schemaContent).toContain('not?: string | undefined')
			expect(schemaContent).toContain('in?: string[] | undefined')
			expect(schemaContent).toContain('notIn?: string[] | undefined')
			expect(schemaContent).toContain('contains?: string | undefined')
			expect(schemaContent).toContain('startsWith?: string | undefined')
			expect(schemaContent).toContain('endsWith?: string | undefined')
		})

		test('NumericFilterInput has correct fields and types', () => {
			expect(schemaContent).toContain('@Field(() => Float, { nullable: true })')
			expect(schemaContent).toContain('equals?: number | undefined')
			expect(schemaContent).toContain('gt?: number | undefined')
			expect(schemaContent).toContain('lt?: number | undefined')
		})

		test('BooleanFilterInput has correct fields', () => {
			expect(schemaContent).toContain('export class BooleanFilterInput')
			expect(schemaContent).toContain('@Field(() => Boolean, { nullable: true })')
		})

		test('Model filter inputs have AND/OR operators', () => {
			expect(schemaContent).toContain('AND?: UserFilterInput[] | undefined')
			expect(schemaContent).toContain('OR?: UserFilterInput[] | undefined')
			expect(schemaContent).toContain('AND?: PostFilterInput[] | undefined')
			expect(schemaContent).toContain('OR?: PostFilterInput[] | undefined')
		})
	})

	// Sort Input Types Tests
	describe('Sort Input Types', () => {
		test('Contains SortDirection enum', () => {
			expect(schemaContent).toContain('export enum SortDirection')
			expect(schemaContent).toContain('ASC = "ASC"')
			expect(schemaContent).toContain('DESC = "DESC"')
			expect(schemaContent).toContain('registerEnumType(SortDirection')
		})

		test('Contains model sort input types', () => {
			expect(schemaContent).toContain('export class UserSortInput')
			expect(schemaContent).toContain('export class PostSortInput')
			expect(schemaContent).toContain('export class CategorySortInput')
			expect(schemaContent).toContain('export class CommentSortInput')
		})

		test('Sort inputs have proper field types', () => {
			expect(schemaContent).toContain('@Field(() => SortDirection, { nullable: true })')
			expect(schemaContent).toContain('createdAt?: SortDirection | undefined')
			expect(schemaContent).toContain('updatedAt?: SortDirection | undefined')
		})

		test('PostSortInput includes viewCount field', () => {
			expect(schemaContent).toContain('viewCount?: SortDirection | undefined')
		})
	})

	// Pagination Input Types Tests
	describe('Pagination Input Types', () => {
		test('Contains pagination input types', () => {
			expect(schemaContent).toContain('export class ForwardPaginationInput')
			expect(schemaContent).toContain('export class BackwardPaginationInput')
			expect(schemaContent).toContain('export class PaginationInput')
		})

		test('ForwardPaginationInput has correct fields', () => {
			expect(schemaContent).toContain('first?: number | undefined')
			expect(schemaContent).toContain('after?: string | undefined')
		})

		test('BackwardPaginationInput has correct fields', () => {
			expect(schemaContent).toContain('last?: number | undefined')
			expect(schemaContent).toContain('before?: string | undefined')
		})

		test('PaginationInput combines all pagination fields', () => {
			const paginationInputMatch = schemaContent.match(/export class PaginationInput[\s\S]*?(?=export class|\n\n@|$)/)
			expect(paginationInputMatch).not.toBeNull()
			const paginationInputSection = paginationInputMatch![0]
			expect(paginationInputSection).toContain('first?: number')
			expect(paginationInputSection).toContain('after?: string')
			expect(paginationInputSection).toContain('last?: number')
			expect(paginationInputSection).toContain('before?: string')
		})
	})

	// Connection Types Tests
	describe('Connection Types', () => {
		test('Contains PageInfo class', () => {
			expect(schemaContent).toContain('export class PageInfo')
			expect(schemaContent).toContain('hasNextPage!: boolean')
			expect(schemaContent).toContain('hasPreviousPage!: boolean')
			expect(schemaContent).toContain('startCursor?: string | undefined')
			expect(schemaContent).toContain('endCursor?: string | undefined')
		})

		test('Contains edge classes for all models', () => {
			expect(schemaContent).toContain('export class UserEdge')
			expect(schemaContent).toContain('export class PostEdge')
			expect(schemaContent).toContain('export class CategoryEdge')
			expect(schemaContent).toContain('export class PostCategoryEdge')
			expect(schemaContent).toContain('export class CommentEdge')
		})

		test('Contains connection classes for all models', () => {
			expect(schemaContent).toContain('export class UserConnection')
			expect(schemaContent).toContain('export class PostConnection')
			expect(schemaContent).toContain('export class CategoryConnection')
			expect(schemaContent).toContain('export class PostCategoryConnection')
			expect(schemaContent).toContain('export class CommentConnection')
		})

		test('Edge classes have correct structure', () => {
			const userEdgeMatch = schemaContent.match(/export class UserEdge[\s\S]*?(?=export class|\n\n@|$)/)
			expect(userEdgeMatch).not.toBeNull()
			const userEdgeSection = userEdgeMatch![0]
			expect(userEdgeSection).toContain('node!: User')
			expect(userEdgeSection).toContain('cursor!: string')
			expect(userEdgeSection).toContain('@Field(() => User)')
			expect(userEdgeSection).toContain('@Field(() => String)')
		})

		test('Connection classes have correct structure', () => {
			const userConnectionMatch = schemaContent.match(/export class UserConnection[\s\S]*?(?=export class|\n\n@|$)/)
			expect(userConnectionMatch).not.toBeNull()
			const userConnectionSection = userConnectionMatch![0]
			expect(userConnectionSection).toContain('pageInfo!: PageInfo')
			expect(userConnectionSection).toContain('edges!: UserEdge[]')
			expect(userConnectionSection).toContain('totalCount!: number')
			expect(userConnectionSection).toContain('@Field(() => PageInfo)')
			expect(userConnectionSection).toContain('@Field(() => [UserEdge])')
			expect(userConnectionSection).toContain('@Field(() => Int)')
		})
	})

	// TypeScript Types Validation
	describe('TypeScript Types Validation', () => {
		test('Uses proper TypeScript number types instead of GraphQL Float', () => {
			const numberFieldMatches = schemaContent.match(/: number/g)
			expect(numberFieldMatches).not.toBeNull()
			expect(numberFieldMatches!.length).toBeGreaterThan(5)
		})

		test('Uses proper TypeScript string array types', () => {
			expect(schemaContent).toContain('string[] | undefined')
			expect(schemaContent).not.toContain('String![] | undefined')
		})

		test('Optional fields have proper TypeScript union with undefined', () => {
			expect(schemaContent).toContain('| undefined')
		})

		test('Required fields have exclamation marks', () => {
			expect(schemaContent).toContain('!: string')
			expect(schemaContent).toContain('!: boolean')
			expect(schemaContent).toContain('!: number')
		})
	})

	// Relations and PostCategory Tests
	describe('Relations and Custom Names', () => {
		test('PostCategory uses correct name from @@graphql.name attribute', () => {
			expect(schemaContent).toContain('export class PostCategory')
			expect(schemaContent).not.toContain('export class CategoryOnPost')
		})

		test('Relations reference correct type names', () => {
			expect(schemaContent).toContain('@Field(() => [PostCategory])')
			expect(schemaContent).toContain('categories!: PostCategory[]')
		})

		test('All models have proper relation fields', () => {
			expect(schemaContent).toContain('@Field(() => [Post])')
			expect(schemaContent).toContain('posts!: Post[]')
			expect(schemaContent).toContain('@Field(() => [Comment])')
			expect(schemaContent).toContain('comments!: Comment[]')
			expect(schemaContent).toContain('@Field(() => User)')
			expect(schemaContent).toContain('author!: User')
		})
	})
})
