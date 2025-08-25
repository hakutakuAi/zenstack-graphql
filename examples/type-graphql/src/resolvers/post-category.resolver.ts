import { Resolver, Query, FieldResolver, Root, Ctx, Arg, Int } from 'type-graphql'
import { PostCategory, Post, Category, PostCategoryFilterInput, PostCategorySortInput, PostCategoryConnection } from '../../schema'
import { BaseResolver } from './base-resolver'
import type { Context } from './types'
import { PRISMA_INCLUDES } from '../utils/resolver-helpers'

@Resolver(() => PostCategory)
export class PostCategoryResolver extends BaseResolver {
	private readonly ALLOWED_FILTER_FIELDS: string[] = []
	private readonly ALLOWED_SORT_FIELDS: string[] = []

	@Query(() => PostCategoryConnection)
	async postCategories(
		@Arg('filter', () => PostCategoryFilterInput, { nullable: true }) filter: PostCategoryFilterInput | null,
		@Arg('sort', () => PostCategorySortInput, { nullable: true }) sort: PostCategorySortInput | null,
		@Arg('first', () => Int, { nullable: true }) first: number | null,
		@Arg('after', () => String, { nullable: true }) after: string | null,
		@Arg('last', () => Int, { nullable: true }) last: number | null,
		@Arg('before', () => String, { nullable: true }) before: string | null,
		@Ctx() { prisma }: Context,
	): Promise<PostCategoryConnection> {
		return this.buildCompositeKeyConnection<PostCategory>(
			prisma,
			'categoryOnPost',
			{ filter, sort, first, after, last, before },
			this.ALLOWED_FILTER_FIELDS,
			this.ALLOWED_SORT_FIELDS,
			(item) => `${item.postId}:${item.categoryId}`,
		)
	}

	@FieldResolver(() => Post)
	async post(@Root() postCategory: PostCategory, @Ctx() { prisma }: Context): Promise<Post | null> {
		return this.findUnique<Post>(prisma, 'post', { id: postCategory.postId }, PRISMA_INCLUDES.POST)
	}

	@FieldResolver(() => Category)
	async category(@Root() postCategory: PostCategory, @Ctx() { prisma }: Context): Promise<Category | null> {
		return this.findUnique<Category>(prisma, 'category', { id: postCategory.categoryId })
	}
}
